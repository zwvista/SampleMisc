<template>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { PostService } from './post.service';
import { inject } from 'vue-typescript-inject';

@Component({
  providers: [
    PostService,
  ],
})
export default class App extends Vue {
  @inject() postService!: PostService;
}
</script>

<style>
</style>
