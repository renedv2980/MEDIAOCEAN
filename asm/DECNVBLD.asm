*          DATA SET DECNVBLD   AT LEVEL 071 AS OF 05/01/02                      
*PHASE DECNBLDA DECNVBLD                                                        
*OUTPUT PHASE TO BUILD TABLES WITH USED NTI/SYN/NHT PRGM NUMBERS                
DECNVBLD CSECT                                                                  
NUMCATS  EQU   38                                                               
         PRINT NOGEN                                                            
         NMOD1 NTIWRKX-NTIWRKD,*OUTBLD*                                         
         USING NTIWRKD,RC          RC=A(TEMP W/S)                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING DECNVBLD+4096,R7    R7=2ND BASE REGISTER                         
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD)                         
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
*                                                                               
         B     *+4(R1)                                                          
         B     MAIN                PROCESS A RECORD                             
         B     CNVX                LAST TIME HOOK (PROCESS YEARLY HUT)          
*                                                                               
XIT      XIT1                                                                   
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
                                                                                
                                                                                
MTA      B     CNVX                                                             
                                                                                
                                                                                
*                                                                               
         OC    INTKEY,INTKEY       UNUSABLE RECORD                              
         BZ    CNVX                                                             
         CLI   INTKEY,C'P'         SHOULD BE A 'P' RECD                         
         BNE   CNVX                                                             
*                                                                               
         BAS   RE,BITREC           1ST RELEASE BIT MAP RECD                     
         BAS   RE,JRECS            THEN RELEASE J-RECD PTRS                     
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*BITREC  - DIVIDES UP INTACCS BYTES INTO 7 (OR 5) ELEMS AND ASSIGNS             
* APPROPRIATE SEQUENCE NUMBER TO ELEMENT                                        
***********************************************************************         
BITREC   NTR1                                                                   
         XC    THISKEY,THISKEY                                                  
         LA    R6,THISKEY                                                       
         USING PRKEY,R6                                                         
         MVI   PRCODE,PRCODEQU     BUILD PNN PPPP-X  WHERE X=N/H/S              
         MVI   PRMEDIA,C'N'                                                     
         MVI   PRSRC,C'N'                                                       
         MVC   PRSTAT,INTSTA                                                    
         MVC   PRSTYP,INTSTYP                                                   
         MVC   PRBOOK,INTBOOK                                                   
         GOTO1 ABLDREC,DMCB,PRKEY                                               
         DROP  R6                                                               
*                                                                               
         LA    R5,INTACCS+1                                                     
         ZIC   R1,INTSTYP          SEQ NUMBER ON RECORD                         
         MH    R1,=H'7'            RECD SEQ * 7 = ELEM SEQ NUMBER               
         STC   R1,SEQ              SLOT INTO ELEMS LATER                        
         LA    R6,ELEM                                                          
         LA    R3,7                GENERATE 7 ELEMS FROM RECD                   
         CLI   INTSTYP,4           LAST RECD?                                   
         BNE   *+8                                                              
         LA    R3,4                ONLY 4 FULL LENGTH ELEMS (253 BYTES)         
*                                                                               
         USING NTIELEM,R6                                                       
BIT10    XC    ELEM,ELEM                                                        
         MVI   NTICODE,NTICODEQ    ELEM = X'04'                                 
         MVI   NTIELN,NTIELNQ      FULL 253 BYTE LENGTH                         
         MVC   NTISEQ,SEQ          ELEM SEQ NUMBER                              
         MVC   NTIBITS,0(R5)       MOVE IN 250 BYTES OF PRG BITS                
         GOTO1 APUTEL,NTIELEM                                                   
         LA    R5,L'NTIBITS(R5)                                                 
         ZIC   R1,SEQ                                                           
         LA    R1,1(R1)                                                         
         STC   R1,SEQ                                                           
         BCT   R3,BIT10            OUTPUT EITHER 5 OR 7 FULL LN ELEMS           
*                                                                               
         CLI   INTSTYP,4           LAST RECD?                                   
         BNE   BITRECX             NO, DONE WITH THIS RECD                      
         XC    ELEM,ELEM                                                        
         MVI   NTICODE,NTICODEQ    ELEM = X'04'                                 
         MVI   NTIELN,NTIELNQL     ONLY 192 BYTE LENGTH                         
         MVC   NTISEQ,SEQ          ELEM SEQ NUMBER                              
         MVC   NTIBITL,0(R5)       MOVE IN 250 BYTES OF PRG BITS                
         GOTO1 APUTEL,NTIELEM                                                   
         DROP  R6                                                               
*                                                                               
BITRECX  DS    0H                  CAN RELASE RECDS AS IT COMES IN              
         GOTO1 APUTTAPE                                                         
                                                                                
         CLC   =C'PNNPPPPN',THISKEY                                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     XIT                 RETURN TO CALL AND GENERATE PTRS             
*                                                                               
* ********************************************************************          
* JRECS-  GENERATE J-RECD PTRS FOR EACH OF THE NTI #'S IN BITMAP                
* ********************************************************************          
JRECS    NTR1                                                                   
         LA    R6,THISKEY                                                       
         USING PJKEY,R6                                                         
         XC    THISKEY,THISKEY                                                  
         MVI   PJCODE,PJCODEQU     JNN- PPPP                                    
         MVI   PJMEDIA,C'N'                                                     
         MVI   PJSRC,C'N'                                                       
         MVC   PJSTAT,INTSTA       INTSTA(4)=PPPP   INTSTA+4=N/H/S              
*                                                                               
         ZIC   R5,INTSTYP          RECD SEQ# (REC=1750 BYTES LAST=1192)         
         MH    R5,=H'1750'         START NTI NUMBER=INTSTYP*1750 BYTES          
         MH    R5,=H'8'            * 8 BITS PER BYTE                            
         LA    R5,1(R5)            R5 = 1ST NTI NUMBER                          
*                                                                               
         L     R1,=F'13999'        14000 PRG NUMBERS ON A REG RECD              
         AR    R1,R5               + START NTI# = LAST NTI# ON THIS REC         
         CLI   INTSTYP,4           EXCEPT ON LAST RECD                          
         BNE   *+8                                                              
         L     R1,=F'65535'        LAST NTI# IS 65535                           
         ST    R1,MAXNTI                                                        
*                                                                               
         SR    R3,R3                                                            
         MVC   WORD,INTACCS+1      ROTATE THRU BITS OF WORDS                    
         LA    R3,INTACCS+1        R3 -> WORD CURRENTLY WORKING WITH            
         ST    R3,AWORD            A(WORD IN INTACCS)                           
         ICM   R3,15,WORD          R3=BITS OF WORD                              
         LA    R0,32               SHIFT THRU 32 BITS IN A WORD                 
*                                                                               
JREC10   TM    WORD,X'80'          GENERATE J-REC FOR THIS NTI#?                
         BO    JREC20              YES                                          
JREC12   SLL   R3,1                TRY NEXT BIT                                 
         STCM  R3,15,WORD                                                       
         LA    R5,1(R5)            BUMP NTI NUMBER                              
         C     R5,MAXNTI           MAX NTI#=65535                               
         BH    JRECX               DONE                                         
         BCT   R0,JREC10                                                        
*                                                                               
         L     R1,AWORD            GET ANOTHER WORD FROM INTACCS                
         LA    R1,4(R1)                                                         
         ST    R1,AWORD                                                         
         MVC   WORD,0(R1)                                                       
         ICM   R3,15,WORD                                                       
         LA    R0,32               RESET LOOP CTR                               
         B     JREC10                                                           
*                                                                               
JREC20   STC   R0,LPCTR            SAVE LOOP COUNTER                            
         STCM  R5,3,PJINTNUM       DDS PROGRAM NUMBER IN HEX                    
         LR    R1,R5                                                            
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         MVC   PJEXTNUM(5),DUB+2   PWOS                                         
         GOTO1 ABLDREC,DMCB,(C'P',PJKEY)                                        
         GOTO1 APUTTAPE            OUTPUT J-REC PTR                             
         SR    R0,R0                                                            
         IC    R0,LPCTR            RESTORE LP CNTR THRU BITS OF WORD            
         B     JREC12              NEXT BIT, NEXT NTI NUMBER                    
*                                                                               
JRECX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* ---------------------------------------------------------------------         
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                  THIS TIME VALUES                             
SPACES   DC    CL10' '                                                          
THISKEY  DC    XL20'00'                                                         
PREVKEY  DC    XL20'00'                                                         
PAVFIL   DC    C'PAVFIL  '         HELLO FILE NAME                              
SORTCARD DC    C'SORT FIELDS=(5,20,A),FORMAT=BI,WORK=1 '                        
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2000,,,,) '                              
         DS    0F                                                               
PAVDIR   DC    CL8'PAVDIR'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMOPEN   DC    CL8'DMOPEN'                                                      
DMSYS    DC    CL8'SPOT'                                                        
DMSYSFLS DC    C'UPAVDIR NPAVFIL X'                                             
*                                                                               
         DS    0F                  CORRECTION RECORDS FIELDS                    
COMMAND  DS    CL8                                                              
KEY      DS    CL23                                                             
KEYSAVE  DS    CL23                                                             
IOSAVE   DS    CL23                SAVE AREA FOR REREAD                         
IOAREA   DS    CL1024                                                           
*                                                                               
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
NTIWRKD  DSECT                                                                  
SEQ      DS    X                                                                
BYTE     DS    X                                                                
LPCTR    DS    X                                                                
NTINUM   DS    H                                                                
WORD     DS    F                                                                
AWORD    DS    F                                                                
MAXNTI   DS    F                                                                
MYWRK    DS    CL11                                                             
ELEM     DS    CL255                                                            
*                                                                               
CONDWORK DS    2000C                                                            
NTIWRKX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DENNHPTD                                                       
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTNT3D                                                      
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071DECNVBLD  05/01/02'                                      
         END                                                                    
