from flask import Flask, render_template, request

import tensorflow as tf
import numpy as np

app = Flask(__name__)

X = tf.placeholder(dtype=tf.float32, shape=[None, 4])
Y = tf.placeholder(dtype=tf.float32, shape=[None, 1])

a = tf.Variable(tf.random_uniform([4,1]), dtype=tf.float32)
b = tf.Variable(tf.random_uniform([1]), dtype=tf.float32)

y = tf.matmul(X, a) + b

saver = tf.train.Saver()

sess = tf.Session()
sess.run(tf.global_variables_initializer())

saver.restore(sess, 'model/saved.ckpt')

@app.route("/", methods=['GET', 'POST'])
def index():
    if request.method == 'GET':
        return render_template('index.html')
    if request.method == 'POST':
        temperature = float(request.form['temperature'])
        rainfall = float(request.form['rainfall'])
        dust = float(request.form['dust'])
        day = float(request.form['day'])
        temp2 = temperature ** 2
        temp3 = temperature ** 3
        data = [[temperature, rainfall, dust, day],]
        new_data = np.array(data, dtype=np.float32)
        result = sess.run(y, feed_dict={X:new_data})
        counts = int(result[0])
        return render_template('index.html', counts = counts)

if __name__ == '__main__':
    app.run(debug= True, use_reloader = False)