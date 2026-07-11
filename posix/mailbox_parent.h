// initialization

int fd = shm_open(name,
                  O_CREAT | O_EXCL | O_RDWR,
                  0600);

ftruncate(fd, shm_size);

void *addr =
    mmap(NULL,
         shm_size,
         PROT_READ|PROT_WRITE,
         MAP_SHARED,
         fd,
         0);

pthread_mutexattr_t attr;

pthread_mutexattr_init(&attr);
pthread_mutexattr_setpshared(&attr,
                             PTHREAD_PROCESS_SHARED);

pthread_mutex_init(&shm->mutex, &attr);

pthread_mutexattr_destroy(&attr);

// Initialize metadata...
static void free_block(struct mailbox_shm *shm, size_t payload_off)
{
  size_t base = payload_off - sizeof(struct value_hdr);

      struct value_hdr *vh =
	(struct value_hdr *)(shm->heap + base);

          size_t total =
	    sizeof(struct value_hdr) + ALIGN8(vh->len);

	      struct free_block *fb =
		(struct free_block *)(shm->heap + base);

	      fb->size = total - sizeof(struct free_block);
	      fb->next = shm->free_list;

	      shm->free_list = base;
}


int mailbox_remove(struct mailbox *mb,
                   pid_t pid,
                   void **out_data,
                   size_t *out_len)
{
    pthread_mutex_lock(&mb->shm->mutex);

    size_t idx = lookup_slot(pid);

    if (!mb->shm->slots[idx].occupied) {
        pthread_mutex_unlock(&mb->shm->mutex);
        return ENOENT;
    }

    struct value_hdr *vh =
        PTR_AT(mb->shm,
               mb->shm->slots[idx].offset);

    void *copy = malloc(vh->len);

    if (!copy) {
        pthread_mutex_unlock(&mb->shm->mutex);
        return ENOMEM;
    }

    memcpy(copy, vh + 1, vh->len);

    *out_data = copy;
    *out_len = vh->len;

    free_block(mb->shm,
               mb->shm->slots[idx].offset);

    mb->shm->slots[idx].occupied = 0;

    pthread_mutex_unlock(&mb->shm->mutex);

    return 0;
}

