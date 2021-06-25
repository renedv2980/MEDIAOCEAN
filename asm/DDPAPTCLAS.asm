*          DATA SET DDPAPTCLAS AT LEVEL 009 AS OF 10/01/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE PAPTCLSA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PANIC                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'PANAPT: REPOPULATE THE LOADMOD NAME VARIATION DATASET'          
PAPTCLS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,PAPTCLS,=V(REGSAVE)                                            
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         OPEN  (SYSPUNCH,OUTPUT)                                                
*                                                                               
         MVC   P(L'INITMSG),INITMSG                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GETREC   DS    0H                                                               
*                                  GET THE NAME RECORD                          
         GOTO1 =V(CARDS),DMCB,INREC,=C'RE00'                                    
         CLC   =C'/*',INREC                                                     
         BE    CLOSE                                                            
*                                                                               
         MVI   INFLAG,C'Y'         WE GOT AT LEAST ONE INPUT RECORD             
         MVC   OUTREC,INREC        INITIALIZE OUTPUT RECORD                     
         MVC   P(L'INREC),INREC                                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
I        USING MEMRECD,INREC                                                    
         GOTO1 =V(PANIC),DMCB,(0,=C'READ'),=C'DIR',I.MEMBRNAM,DIRECT2           
         CLI   DMCB+8,0            PAN BOOK FOUND?                              
         BE    *+12                                                             
         MVI   PANFLAG,C'N'        NO: WHAT HAPPENED TO IT?                     
         B     CLOSE                                                            
*                                                                               
         MVC   TESTNAME,SPACES                                                  
         MVC   BASENAME,SPACES                                                  
         MVC   BKUPNAME,SPACES                                                  
         MVC   TEMPNAME,SPACES                                                  
         MVI   XXGSW,C'N'          POSIT:  NOT APG/DPG/RRG                      
         MVI   DICTSW,C'N'         *DICT NOT YET FOUND                          
*                                                                               
GETPAN   DS    0H                                                               
         GOTO1 =V(PANIC),DMCB,(0,=C'READ'),=C'PAN',I.MEMBRNAM,RECORD            
         CLI   DMCB+8,0            TEST PAN BOOK FOUND                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(L'RECORD),RECORD                                               
         GOTO1 =V(PRINTER)                                                      
         CLC   =C'$*',RECORD       END OF MEMBER?                               
         BNE   GETPAN10            NO: PROCESS THIS RECORD                      
         MVC   MSG1NM,I.MEMBRNAM                                                
         MVC   P(MSG1LQ),MSG1                                                   
         GOTO1 =V(PRINTER)                                                      
         B     GETREC              GET NEXT SYSIN RECORD                        
         DROP  I                                                                
*                                                                               
GETPAN10 DS    0H                                                               
         CLI   XXGSW,C'Y'          XXG IDENTIFIED?                              
         BE    XXGPHASE            YES: FIND PHASE CARD                         
*                                                                               
         CLC   =C'*PHASE ',RECORD  IS IT A "PHASE" CARD?                        
         BE    PROG                YES: GENERATE PROG CARDS                     
*                                                                               
         CLC   =C'*DICT ',RECORD   IS IT A "*DICT" CARD?                        
         BNE   CHKDONE             NO: SEE IF WE ARE DONE                       
         MVI   DICTSW,C'Y'         YES: SET "*DICT FOUND"                       
         LA    R3,RECORD+6         ADVANCE PAST *DICT                           
         B     PROG10              GENERATE CONTROL CARDS                       
*                                                                               
CHKDONE  DS    0H                                                               
         CLI   DICTSW,C'Y'         WERE WE DOING *DICT CARDS?                   
         BNE   CHKAPG              NO: CHECK FOR *APG                           
         MVI   DICTSW,C'N'         YES: RESET SWITCH                            
         B     GETREC              GET NEXT SYSIN RECORD                        
*                                                                               
CHKAPG   DS    0H                                                               
         CLC   =C'*APG ',RECORD    IS IT APG?                                   
         BNE   CHKDPG              NO: CHECK FOR DPG                            
         MVC   XXGADDR,=A(APGRTN)                                               
         MVI   XXGSW,C'Y'          LOOK FOR PHASE CARD NEXT                     
         B     GETPAN              GET NEXT RECORD                              
*                                                                               
CHKDPG   DS    0H                                                               
         CLC   =C'*DPG ',RECORD    IS IT DPG?                                   
         BNE   CHKRRG              NO: CHECK FOR RRG                            
         MVC   XXGADDR,=A(DPGRTN)                                               
         MVI   XXGSW,C'Y'          LOOK FOR PHASE CARD NEXT                     
         B     GETPAN              GET NEXT RECORD                              
*                                                                               
CHKRRG   DS    0H                                                               
         CLC   =C'*RRG ',RECORD    IS IT RRG?                                   
         BNE   CHKGEN              NO: CHECK FOR SCRGEN                         
         MVC   XXGADDR,=A(RRGRTN)                                               
         MVI   XXGSW,C'Y'          LOOK FOR PHASE CARD NEXT                     
         B     GETPAN              GET NEXT RECORD                              
*                                                                               
CHKGEN   DS    0H                                                               
         CLC   =C'*GEN ',RECORD    IS IT SCRGEN?                                
         BNE   GETPAN              NO: GET NEXT RECORD                          
         MVC   XXGADDR,=A(GENRTN)                                               
         MVI   XXGSW,C'Y'          LOOK FOR SCREEN CARD NEXT                    
         B     GETPAN              GET NEXT RECORD                              
*                                                                               
XXGPHASE DS    0H                  SCAN FOR "PHASE"                             
         CLI   RECORD,C'*'         COMMENT CARD?                                
         BE    GETPAN              YES: SKIP IT                                 
*                                                                               
         LA    R3,RECORD                                                        
         L     RF,XXGADDR          ADDR OF TYPE-SPECIFIC RTN                    
         BR    RF                                                               
         EJECT                                                                  
GENRTN   DS    0H                                                               
         CLI   RECORD,C'S'         SCREEN CARD?                                 
         BNE   GETPAN              GET NEXT RECORD                              
*                                                                               
         MVC   TESTNAME(4),RECORD+4   START OF BASE NAME (SSPP)                 
         MVC   TESTNAME+4(2),RECORD+1    BRING IN OVERLAY # (OO)                
         MVC   BASENAME,TESTNAME   COPY BASE NAME                               
         MVC   BKUPNAME,TESTNAME   COPY BASE NAME                               
         MVI   BKUPNAME+6,C'S'     CREATE "SAVE" NAME                           
         MVC   TEMPNAME,TESTNAME   COPY BASE NAME                               
         MVI   TEMPNAME+6,C'@'     CREATE "TEMP" NAME                           
         CLI   RECORD+3,C' '       TEST SUFFIX DEFAULTED?                       
         BNE   *+8                 NO: USE IT AS GIVEN                          
         MVI   RECORD+3,C'A'       SUPPLY DEFAULT                               
         MVC   TESTNAME+6(1),RECORD+3    MOVE IN SUFFIX CHAR                    
         B     CMDS                GO ISSUE PROG CARDS                          
*                                                                               
APGRTN   DS    0H                                                               
         CLC   =C'PHASE    ',RECORD    APG PHASE RECORD?                        
         BNE   GETPAN              NO: GET NEXT                                 
*                                                                               
         MVC   TESTNAME(2),=CL2'AC'                                             
         MVC   TESTNAME+2(6),RECORD+9  CONSTRUCT PHASE NAME                     
         B     OTHRNM              BUILD OTHER NAMES                            
*                                                                               
DPGRTN   DS    0H                                                               
         CLC   =C'         PHASE ',RECORD     DPG PHASE RECORD?                 
         BNE   GETPAN              NO: GET NEXT                                 
*                                                                               
         MVC   TESTNAME,RECORD+15  RETRIEVE PHASE NAME                          
         B     OTHRNM              BUILD OTHER NAMES                            
*                                                                               
RRGRTN   DS    0H                                                               
         CLC   =C'PHASE    ',RECORD   RRG PHASE RECORD?                         
         BNE   GETPAN              NO: GET NEXT                                 
*                                                                               
         MVC   TESTNAME(4),=CL4'RERG'    CONSTRUCT PHASE NAME                   
         MVC   TESTNAME+4(3),RECORD+9                                           
*                                        BUILD OTHER NAMES                      
*                                                                               
OTHRNM   DS    0H                  BUILD BASE AND BKUP NAMES                    
         LA    R3,TESTNAME         SEEK BLANK                                   
         SR    R1,R1               CLEAR FOR TRT                                
         TRT   TESTNAME+1(8),TRTBLANK                                           
         SR    R1,R3                                                            
         STH   R1,NAMELN                                                        
         BCTR  R1,0                OMIT SUFFIX LETTER                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BASENAME(0),TESTNAME                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BKUPNAME(0),TESTNAME                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPNAME(0),TESTNAME                                             
         LH    R1,NAMELN                                                        
         LA    RF,BKUPNAME(R1)     POINT BEYOND NAME                            
         BCTR  RF,0                BACK UP TO LAST CHAR                         
         MVI   0(RF),C'S'          CREATE "SAVE" NAME                           
         LA    RF,TEMPNAME(R1)     POINT BEYOND NAME                            
         BCTR  RF,0                BACK UP TO LAST CHAR                         
         MVI   0(RF),C'@'          CREATE "TEMP" NAME                           
         B     CMDS                                                             
*                                                                               
PROG     DS    0H                                                               
         LA    R3,RECORD+7         ADVANCE PAST *PHASE                          
*                                                                               
PROG10   DS    0H                  FIND START OF NAME                           
         CLI   0(R3),C' '                                                       
         BNE   *+12                FOUND FIRST NONBLANK                         
         LA    R3,1(R3)            ADVANCE POINTER                              
         B     PROG10              LOOP                                         
*                                  START OF NAME                                
         LA    R4,1(R3)                                                         
PROG22   DS    0H                                                               
         CLI   0(R4),C' '          FIND END OF NAME                             
         BE    PROG30                                                           
         CLI   0(R4),C','          FIND END OF NAME                             
         BE    PROG30                                                           
         LA    R4,1(R4)                                                         
         B     PROG22                                                           
PROG30   DS    0H                                                               
         LR    R1,R4               KEEP R4                                      
         SR    R1,R3               LENGTH OF NAME                               
         STH   R1,NAMELN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TESTNAME(0),0(R3)                                                
*                                                                               
         LH    R1,NAMELN                                                        
         BCTR  R1,0                OMIT SUFFIX LETTER                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BASENAME(0),TESTNAME                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BKUPNAME(0),TESTNAME                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPNAME(0),TESTNAME                                             
         LH    R1,NAMELN                                                        
         LA    RF,BKUPNAME(R1)     POINT BEYOND NAME                            
         BCTR  RF,0                BACK UP TO LAST CHAR                         
         MVI   0(RF),C'S'          CREATE "SAVE" NAME                           
         LA    RF,TEMPNAME(R1)     POINT BEYOND NAME                            
         BCTR  RF,0                BACK UP TO LAST CHAR                         
         MVI   0(RF),C'@'          CREATE "TEMP" NAME                           
         EJECT                                                                  
CMDS     DS    0H                                                               
*                                                                               
*  CREATE OUTPUT RECORDS FOR INPUT TO REXX UTILITY.                             
*                                                                               
O        USING MEMRECD,OUTREC                                                   
         MVC   O.MEMTSTNM,TESTNAME TEST MEMBER NAME                             
         MVC   O.MEMBASNM,BASENAME DERIVED BASE NAME                            
         MVC   O.MEMBAKNM,BKUPNAME DERIVED BKUP NAME                            
         MVC   O.MEMTMPNM,TEMPNAME DERIVED TEMP NAME                            
         DROP  O                                                                
*                                                                               
         MVC   P(L'OUTREC),OUTREC                                               
         GOTO1 =V(PRINTER)                                                      
         PUT   SYSPUNCH,OUTREC                                                  
         MVI   PUTFLAG,C'Y'        REMEMBER THAT WE PUT SOMETHING               
*                                                                               
         CLI   DICTSW,C'Y'         WAS THIS A *DICT CARD?                       
         BNE   GETREC              NO: GET NEXT SYSIN RECORD                    
         MVC   OUTREC,INREC        REINITIALIZE OUTPUT RECORD                   
         BE    GETPAN              YES: THERE MAY BE MORE                       
*                                                                               
CLOSE    DS    0H                                                               
         CLOSE (SYSPUNCH)                                                       
         MVC   P(L'TERMMSG),TERMMSG                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(PANIC),DMCB,=C'CLOSE',=C'PAN'                                 
*                                                                               
         CLI   INFLAG,C'Y'         ANY INPUT RECORDS READ?                      
         BE    CHKPAN                                                           
         MVC   RETCODE,=F'12'      NO: SET RC=12                                
         MVC   P(L'NOINMSG),NOINMSG                                             
         GOTO1 =V(PRINTER)                                                      
         B     RETURN              SYSIN WAS EMPTY: SHOULD NEVER HAPPEN         
*                                                                               
CHKPAN   DS    0H                                                               
         CLI   PANFLAG,C'N'        A PANVALET MEMBER WENT MISSING?              
         BNE   CHKPUNCH                                                         
         MVC   RETCODE,=F'12'      NO: SET RC=12                                
         MVC   P(L'NOPANMSG),NOPANMSG                                           
         GOTO1 =V(PRINTER)                                                      
         B     RETURN              USER PROBABLY MUCKED WITH IT                 
*                                                                               
CHKPUNCH DS    0H                                                               
         CLI   PUTFLAG,C'Y'        ANY RECORDS PUT TO SYSPUNCH?                 
         BE    RETURN                                                           
         MVC   RETCODE,=F'12'      NO: SET RC=12                                
         MVC   P(L'NOOUTMSG),NOOUTMSG                                           
         GOTO1 =V(PRINTER)                                                      
         B     RETURN              DID USER MUCK WITH THE MR?                   
*                                                                               
RETURN   DS    0H                                                               
         XBASE RC=RETCODE                                                       
         EJECT                                                                  
*************************************************************                   
*                                                                               
*   WORK AREAS                                                                  
*                                                                               
*************************************************************                   
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
RETCODE  DC    F'0'                PROGRAM RETURN CODE                          
*                                                                               
XXGSW    DC    C'N'                                                             
DICTSW   DC    C'N'                                                             
*                                                                               
INFLAG   DC    C'N'                'Y' IF ANY INPUT RECORDS WERE READ           
PANFLAG  DC    C'Y'                'N' IF A PANVALET MEMBER IS MISSING          
PUTFLAG  DC    C'N'                'Y' IF WE PUT ANYTHING TO SYSPUNCH           
*                                                                               
XXGADDR  DS    A                                                                
*                                                                               
         DS    0D                                                               
TRTBLANK DC    256X'00'                                                         
         ORG   TRTBLANK+C' '                                                    
         DC    X'FF'                                                            
         ORG   TRTBLANK+C','                                                    
         DC    X'FF'                                                            
         ORG                                                                    
*                                                                               
RECORD   DC    CL80' '                                                          
DIRECT2  DS    CL80                                                             
*                                                                               
NAMELN   DS    H                                                                
TESTNAME DC    CL8' '              STGE PHASE NAME                              
         DC    C' '                 (DELIMIT PREVIOUS FIELD)                    
BKUPNAME DC    CL8' '              BACKUP PHASE NAME                            
BASENAME DC    CL8' '              PRODUCTION PHASE NAME                        
TEMPNAME DC    CL8' '              TEMPORARY PHASE NAME (FOR RENAMES)           
*                                                                               
INREC    DS    CL80                INPUT RECORD                                 
OUTREC   DS    CL80                OUTPUT RECORD                                
*                                                                               
INITMSG  DC    C'BEGINNING EXECUTION'                                           
TERMMSG  DC    C'TERMINATING EXECUTION'                                         
NOINMSG  DC    C'*** FATAL ERROR *** NO INPUT RECORDS FOUND'                    
NOPANMSG DC    C'*** FATAL ERROR *** PANVALET MEMBER NOT FOUND'                 
NOOUTMSG DC    C'*** FATAL ERROR *** NO OUTPUT RECORDS WRITTEN'                 
*                                                                               
MSG1     DC    C'PAPTCLAS1-01: '                                                
MSG1NM   DS    CL10                                                             
         DC    C' TYPE NOT DETERMINED.'                                         
MSG1LQ   EQU   *-MSG1                                                           
*                                                                               
SYSPUNCH DCB   DDNAME=SYSPUNCH,DSORG=PS,LRECL=80,RECFM=FB,MACRF=PM              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* THIS STRUCTURE IS READ BY THE JRXUTIL CLIST AND BY DDPAPTMRM.                 
* IT IS ALSO READ BY THE JCLMCOMP CLIST.                                        
*                                                                               
         DSECT                                                                  
MEMRECD  DS    0CL80                                                            
MEMBRNAM DS    CL10                SOURCE MEMBER NAME                           
         DS    CL2                                                              
MEMTSTNM DS    CL8                 TEST LOADMOD NAME                            
         DS    CL2                                                              
*                                                                               
* THE NEXT TWO FIELDS ARE ALWAYS CONSTRUCTED WITHIN THIS PROGRAM. WE            
* NO LONGER SUPPORT THEM ON INPUT FROM INVENTORY RECORDS. (WE USED TO           
* HAVE THE ABILITY TO OPTIONALLY STORE THESE NAMES IN FIELDS                    
* DIBSUSER01 AND DIBSUSER02, RESPECTIVELY.)                                     
*                                                                               
MEMBASNM DS    CL8                 BASE LOADMOD NAME                            
         DS    CL2                                                              
MEMBAKNM DS    CL8                 BACKUP LOADMOD NAME                          
         DS    CL2                                                              
         DS    CL7                 LIBRARY CODE                                 
         ORG   *-7                                                              
         DS    CL4                 LIBCODE                                      
         DS    CL3                 SUBCODE                                      
         DS    C                                                                
         DS    CL6                 *** PROBABLY UNNEEDED (SEE JRXUTIL)          
         DS    CL3                                                              
         DS    CL4                 'YES' IF LIBCODE USES STGE, OW 'NO'          
         DS    C                                                                
MEMTMPNM DS    CL8                 TEMPORARY LOADMOD NAME (FOR RENAMES)         
         DS    CL(L'MEMRECD-(*-MEMRECD))                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDPAN0UPD                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DDPAPTCLAS10/01/14'                                      
         END                                                                    
