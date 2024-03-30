class BinTree:
    def __init__(self):
        self.data = self.lchild = self.rchild = None

    # insert, lookup, etc.

    def preorder(self):
        if self.data != None:
            yield self.data
        if self.lchild != None:
            for d in self.lchild.preorder():
                yield d
        if self.rchild != None:
            for d in self.rchild.preorder():
                yield d

    def inorder(self):
       if self.lchild != None:
            for d in self.lchild.preorder():
                yield d
        if self.data != None:
            yield self.data
        if self.rchild != None:
            for d in self.rchild.preorder():
                yield d

    def postorder(self):
       if self.lchild != None:
            for d in self.lchild.preorder():
                yield d
        if self.rchild != None:
            for d in self.rchild.preorder():
                yield d
        if self.data != None:
                yield self.data
