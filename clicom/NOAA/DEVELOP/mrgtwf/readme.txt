

                          KEY-ENTRY FILES MERGER


   The MRGTWF program is designed to merge two key-entry files into
one file and allows user to display the table of contents for key-entry
to screen or printer.  

I. INSTALLATION KEY-ENTRY FILE MERGER

    INST-TWF is the procedure that install the MRGTWF program and its related
files.  During the installation process INST-TWF:

       1. Save the files MESSAGES.TXT, MESSAGES.FTN, and          
          USERMENU.DEF under MESSAGES.TSV, MESSAGES.FSV, and      
          USERMENU.DSV

       2. Copies files into CLICOM directory:

          - Program:             MRGTWF.EXE
          - Help:                MRGTWF.HLP
          - Message:             MESSAGES.TXT, MESSAGES.FTN
          - Froms:               MRGDUPID.FRM, MRGTWFIO.FRM, LSTKEFRM.FRM,
                                 and PRNLSTWF.FRM
          - Menus:               USERMENU.DEF, LIST/MRG-TWF.MNU, and
                                 KE-OUTPUT.MNU
          - Parameter file:      PRINTWF.PRM

       Installation procedure:

       1. Place the installation diskette into drive A or B.  If you select
          drive B, substitute the letter B whenever a reference is made to
          drive A in the instructions.
 
       2. At the DOS prompt, enter the command:
              A:INST-TWF A    or    B:INST-TWF B

       3. After the request, enter the letter of the drives where your
          CLICOM and CLIGRAF directories are installed.

       4. Check the drive letters that are displayed, then enter the
          appropriate response:

              Press [Enter] to continue the installation
              Press [Esc] to revise the drive letters
              Press [Q] to quit the installation

       5. The installation displayed the message, 'Working', while it saves
          and copies the required files. 

II. KEY-ENTRY FILES MERGER INSTRUCTIONS

    A. Procedure to run the MRGTWF program

       1. Begin CLICOM

       2. Select choice #7 (Enter a single DOS command) from the CLICOM
          Introductory Menu.

       3. At the prompt, enter the command: MRGTWF     Enter

       4. The menu (Merge Key-Entry Files) is displayed with the following
          choices:

               0  Exit
               1  Merge two key-entry files
               2  List key-entry record ID's 
               3  View a key-entry form
               4  List current key-entry forms

       5. Select choice #1 (Merge two key-entry files) to merge two key-entry
          files into one file.

       6. Select choice #2 (List key-entry record ID's) to display record ID's
          in selected key-entry file.  The program only display the station-ID
          and dates which saved in the key-entry file. 

       7. Select choice #3 (View a key-entry form) to display the element
          codes which defined in the key-entry form.

       8. Select choice #4 (List current key-entry forms) to display all of
          the key-entry forms which are available in CLICOM.

       9. Select choice #0 to exit the KEY-ENTRY FILE MERGER (MRGTWF) program
          and return to CLICOM.

       For a additional information on the program, please read the help
file P:\HELP\MRGTWF.HLP.