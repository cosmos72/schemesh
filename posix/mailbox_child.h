static size_t alloc_block(struct mailbox_shm *shm, size_t size)
{
  size = ALIGN8(size);

  size_t *prev = &shm->free_list;
  size_t cur = shm->free_list;

  /* 1. First-fit search in free list */
  while (cur != NULL_OFF) {
            struct free_block *fb =
	      (struct free_block *)(shm->heap + cur);

	    if (fb->size >= size) {

	      /* remove from free list */
	      *prev = fb->next;

	      /* optionally split block */
	      size_t remaining = fb->size - size;

	      if (remaining > sizeof(struct free_block) + 8) {
		size_t new_off = cur + sizeof(struct free_block) + size;

		                struct free_block *newfb =
				  (struct free_block *)(shm->heap + new_off);

				newfb->size = remaining - sizeof(struct free_block);
				newfb->next = shm->free_list;

				shm->free_list = new_off;
	      }

	      return cur + sizeof(struct free_block);
	    }

	    prev = &fb->next;
	    cur = fb->next;
  }

  /* 2. Bump allocation */
  size_t off = shm->bump;

  size_t total = sizeof(struct value_hdr) + size;

  if (off + total > shm->heap_size) {
    return NULL_OFF;
  }

  shm->bump += total;

  return off + sizeof(struct value_hdr);
}


int mailbox_put(struct mailbox *mb,
                pid_t pid,
                const void *data,
                size_t len)
{
    pthread_mutex_lock(&mb->shm->mutex);

    size_t idx = lookup_slot(pid);

    if (mb->shm->slots[idx].occupied) {
        pthread_mutex_unlock(&mb->shm->mutex);
        return EEXIST;
    }

    size_t off =
        alloc_block(mb->shm,
                    sizeof(struct value_hdr)+len);

    if (!off) {
        pthread_mutex_unlock(&mb->shm->mutex);
        return ENOMEM;
    }
x
    struct value_hdr *vh =
        PTR_AT(mb->shm, off);

    vh->pid = pid;
    vh->len = len;
    vh->used = 1;

    memcpy(vh + 1, data, len);

    mb->shm->slots[idx].pid = pid;
    mb->shm->slots[idx].offset = off;
    mb->shm->slots[idx].occupied = 1;

    pthread_mutex_unlock(&mb->shm->mutex);

    return 0;
}
