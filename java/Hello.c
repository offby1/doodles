#include <jni.h>
#include "HelloWorld.h"
#include <stdio.h>

JNIEXPORT void JNICALL
Java_HelloWorld_doSomethingMoreInteresting (JNIEnv *env,
                                            jobject this,
                                            jobject callback,
                                            jobject data)
{
  jclass cls = (*env)->GetObjectClass(env, callback);
  jmethodID mid = (*env)->GetMethodID(env,
                                      cls,
                                      "DoIt",
                                      "(IILjava/lang/String;)Z");
  if (mid == 0) {
    fprintf (stderr, "Oh hell; GetMethodID failed.\n");
    return;
  }

  jboolean status = (*env)->CallBooleanMethod(env, callback, mid, 1, 2, data);
  if (status) {
    printf ("Callback returned true\n");
  } else {
    printf ("Callback returned false\n");
  }

  status = (*env)->CallBooleanMethod(env, callback, mid, 2, 2, data);
  if (status) {
    printf ("Callback returned true\n");
  } else {
    printf ("Callback returned false\n");
  }
}
