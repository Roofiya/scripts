1. Create a remote repository
Log in to GitHub, then click on the icon in the top right corner to create a new repository called scripts:

2. Connect local to remote repository
git remote add origin git@github.com:Roofiya/scripts.git
git remote -v

3. Set SSH up, let’s check our authentication again from the command line.
ssh -T git@github.com

4. Push local changes to a remote
git push -u origin master
