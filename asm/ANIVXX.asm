*          DATA SET ANIVXX     AT LEVEL 001 AS OF 07/28/98                      
*PHASE SPIV02A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPIV02 - PAID LISTING BY MKT GROUP'                             
SPIV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPIV02,R8                                                      
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    IV0100                                                           
         XIT1                      ALL DONE                                     
*                                                                               
IV0100   EQU   *                                                                
*                                                                               
         MVC   MYAGY,BAGYMD        AGENCY/MEDIA                                 
         NI    MYAGY,X'F0'         AGENCY ONLY IN HIGH ORDER                    
         MVI   FIRSTREC,1          SET FOR 'FIRST RECORD'                       
         LA    R0,MYHEAD           A(HEADLINE ROUTINE)                          
         ST    R0,HEADHOOK         AND STORE IT                                 
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD INITIALIZE SORTER               
*                                                                               
* GET THE AGENCY LEVEL AOA PROFILE FOR THE MARKET GROUP SCHEME CODE             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SA0A'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),QAGY                                                   
         GOTO1 GETPROF,DMCB,WORK,PROFILE,DATAMGR                                
*                                                                               
         CLI   PROFILE+11,C' '     MARKET GROUP SCHEME CODE THERE?              
***      BNH   IVEXIT              NO - SO EXIT                                 
         BH    *+6                                                              
         DC    H'0'                DUMP FOR NOW                                 
*                                                                               
* NOW READ THE MARKET GROUP PASSIVE KEYS FOR THIS SCHEME CODE AND               
* STORE THE GROUP NUMBER IN THE INDEXED LIST.                                   
*                                                                               
IV0150   EQU   *                                                                
*                                                                               
         XCEF  MKGLIST,20000                                                    
         XC    KEY,KEY             INITIALIZE THE KEY                           
         LA    R6,KEY                                                           
         USING MKGRECD,R6                                                       
         MVC   MKGPTYP,=X'0D82'    PASSIVE KEY RECORD TYPE                      
         MVC   MKGPAGMD,MYAGY      AGENCY CODE IN HIGH ORDER NIBBLE             
         OC    MKGPAGMD,NEWMED     MEDIA CODE IN LOW ORDER                      
         GOTO1 HIGH                READ THE KEY                                 
         B     IV0250              JUMP OVER SEQ                                
*                                                                               
IV0200   EQU   *                                                                
*                                                                               
         GOTO1 SEQ                 GET THE NEXT KEY                             
*                                                                               
IV0250   EQU   *                                                                
*                                                                               
         CLC   MKGPTYP,KEYSAVE     SAME RECORD TYPE?                            
         BNE   IV0300              NO - SO GO READ THE AUTOPAY RECS             
*                                                                               
         MVC   BYTE,MKGPAGMD       GET THE AGY/MEDIA                            
         NI    BYTE,X'F0'          TURN OFF THE MEDIA                           
         CLC   MYAGY,BYTE          SAME AGY CODE?                               
         BNE   IV0300              NO - SO GO READ THE AUTOPAY RECS             
*                                                                               
         MVC   BYTE,MKGPAGMD       ELSE - CHECK FOR MEDIA BREAK                 
         NI    BYTE,X'0F'          TURN OFF THE AGENCY                          
         OC    MYMED,MYMED         1ST PASS FOR THIS MEDIUM?                    
         BNZ   IV0260              NO - SO CONTINUE                             
*                                                                               
         MVC   MYMED,BYTE          SAVE THE MEDIA                               
         B     IV0270              AND CONTINUE                                 
*                                                                               
IV0260   EQU   *                                                                
*                                                                               
         CLC   BYTE,MYMED          SAME MEDIA?                                  
         BE    IV0270              YES - SO CONTINUE                            
*                                                                               
         MVC   NEWMED,BYTE         ELSE - SAVE NEW MEDIA                        
         B     IV0300              AND GO READ THE AUTOPAY RECS                 
*                                                                               
IV0270   EQU   *                                                                
*                                                                               
         CLC   MKGPMID,PROFILE+11  ELSE - SAME GROUP SCHEME CODE                
         BNE   IV0200              NO - SO GET NEXT KEY                         
*                                                                               
         MVI   MKGFLAG,1           SET THE 'FOUND A MKGREC' FLAG                
         LA    R5,MKGLIST          ELSE - GET A(MARKET LIST)                    
         ZICM  R4,MKGPMKT,2        GET THE MARKET NUMBER                        
         SLL   R4,1                DOUBLE IT TO GET INDEX INTO LIST             
         AR    R5,R4               A(ACTUAL CELL IN LIST)                       
         MVC   0(2,R5),MKGPMGRP    MOVE MARKET GROUP NUMBER TO LIST             
*                                                                               
         B     IV0200              AND GET NEXT KEY                             
         DROP  R6                                                               
*                                                                               
* NOW READ THE AUTOPAY CLEARANCE RECORDS FOR THE REST OF THE DATA               
*                                                                               
IV0300   EQU   *                                                                
*                                                                               
         OC    MKGFLAG,MKGFLAG     ANY MKGRECS FOUND?                           
         BZ    IV0700              NO - SO SORT WHAT WE'VE ALREADY GOT          
*                                                                               
         XC    KEY,KEY             INITIALIZE THE KEY                           
         LA    R6,KEY                                                           
         USING APYRECD,R6                                                       
         MVI   APYKTYP,APYKTYPQ    RECORD TYPE '0D'                             
         MVI   APYKSUB,APYKSUBQ    SUB TYPE '3A'                                
         MVC   APYKDATE,MYDATE     0 FIRST TIME THRU                            
         MVC   APYKAGMD,MYAGY      AGY CODE IN HIGH ORDER                       
         OC    APYKAGMD,MYMED      MEDIA IN LOW ORDER                           
*                                                                               
         GOTO1 HIGH                                                             
         B     IV0450              JUMP OVER SEQ                                
*                                                                               
IV0400   EQU   *                                                                
*                                                                               
         LA    R6,KEY              RESTORE A(KEY) J.I.C                         
         GOTO1 SEQ                                                              
*                                                                               
IV0450   EQU   *                                                                
*                                                                               
         CLC   APYKTYP(2),KEYSAVE  SAME RECORD/SUB TYPE?                        
         BNE   IV0700              NO - SO GO SORT 'EM                          
*                                                                               
         OC    MYDATE,MYDATE       FIRST TIME THRU?                             
         BNZ   IV0475              NO - SO CHECK DATE                           
*                                                                               
         MVC   MYDATE,APYKDATE     ELSE - SAVE DATE FOR NEXT PASS               
         B     IV0500              AND CONTINUE                                 
*                                                                               
IV0475   EQU   *                                                                
*                                                                               
         CLC   MYDATE,APYKDATE     SAME DATE?                                   
         BNE   IV0700              NO - SO GO SORT 'EM                          
*                                                                               
IV0500   EQU   *                                                                
*                                                                               
         CLC   APYKAGMD,KEYSAVE+4  SAME AGENCY/MEDIA?                           
         BNE   IV0650              NO - SO GO READ NEXT MKG RECS                
*                                                                               
         GOTO1 GETBUY              READ AUTOPAY REC INTO BUY IO AREA            
         L     R6,ADBUY            R6 = A(BUY) - STILL HAS SAME USING           
*                                                                               
         CLC   APYPAID,=H'0'       ANY PAID DATE?                               
         BE    IV0400              NO - SO GET NEXT KEY                         
*                                                                               
         XC    SORTREC,SORTREC     INITIALIZE THE SORT RECORD                   
         LA    R7,SORTREC                                                       
         USING SORTRECD,R7                                                      
*                                                                               
* NOW BUILD THE SORT KEY                                                        
*                                                                               
         PACK  DUB,APYMKT(L'APYMKT) SET UP MARKET CONVERSION                    
         CVB   R1,DUB              TO HEX                                       
         SLL   R1,1                DOUBLE MKT # TO GET INDEX                    
         LA    R5,MKGLIST          A(INDEXED MKT LIST)                          
         AR    R5,R1               A(ACTUAL CELL IN LIST)                       
         MVC   SRMKTGR,0(R5)       MARKET GROUP NUMBER                          
*                                                                               
         MVC   SRMKT,APYMKT        MARKET NUMBER                                
         MVC   SRSTA,APYSTA        STATION CALL LETTERS                         
         MVC   SRMED,APYMED        MEDIA                                        
         MVC   SRCLT,APYCLT        CLIENT                                       
         MVC   SRPRD,APYPRD        PRODUCT                                      
         MVC   SRPRD2,APYPRD2      PIGGY-WIGGY                                  
         MVC   SREST,APYEST        ESTIMATE                                     
*                                                                               
* AND THE SORT RECORD                                                           
*                                                                               
         MVC   SRPAID,APYPAID      PAID DATE                                    
*                                                                               
         MVI   ELCODE,APYIELQ      INVOICE (X'02') ELEMENT                      
         BRAS  RE,GETEL            GET THE ELEMENT                              
***      BNE   IV0400              NOT FOUND - SO GET NEXT KEY                  
         BE    *+6                 FOUND IT - CONTINUE                          
         DC    H'0'                DUMP (FOR TESTING ONLY)                      
*                                                                               
         USING APYIEL,R6                                                        
         MVC   SRINVNUM,APYINUM    NUMBER OF INVOICES                           
         ZIC   R1,APYINUM          LOOP COUNTER                                 
         LA    R6,APYIINV          A(INVOICE NUMBER)                            
         LA    R5,SRINVLST         A(LIST IN SORT REC)                          
*                                                                               
IV0600   EQU   *                                                                
*                                                                               
         MVC   0(10,R5),0(R6)      MOVE IN THE INVOICE NUMBER                   
         LA    R5,L'SRINVLST(R5)   INC TO NEXT INV NUMBER                       
         LA    R6,APYILENQ(R6)        "    "      "                             
         BCT   R1,IV0600           GO GET THE NEXT ONE                          
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R7) WRITE THE REC TO SORT               
         MVI   DATA,1              SET 'DATA FOUND' FLAG                        
         B     IV0400              AND GO GET NEXT AUTOPAY KEY                  
         DROP  R6,R7                                                            
*                                                                               
IV0650   EQU   *                                                                
*                                                                               
         XC    MYMED,MYMED         CLEAR THE CURRENT MEDIA                      
         XC    MKGFLAG,MKGFLAG     CLEAR THE 'FOUND A MKGREC' FLAG              
         B     IV0150              AND GO GET NEXT MKGREC                       
*                                                                               
* READ THE SORTED RECORDS BACK AND PRINT 'EM OUT                                
*                                                                               
IV0700   EQU   *                                                                
*                                                                               
         LA    R4,P                A(PRINT LINE)                                
         USING PRINTD,R4                                                        
*                                                                               
IV0800   EQU   *                                                                
*                                                                               
         OC    DATA,DATA           ANY SORT RECORDS WRITTEN?                    
         BZ    IVEXIT              NO - SO ALL DONE                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R5,15,4(R1)         E.O.F?                                       
         BZ    IVEXIT              YES - SO ALL DONE                            
*                                                                               
         ST    R5,ASORTREC         FOR USE BY MYHEAD ROUTINE                    
         USING SORTRECD,R5                                                      
         OC    FIRSTREC,FIRSTREC   FIRST RECORD?                                
         BNZ   IV0850              YES - SO CONTINUE                            
*                                                                               
         CLC   SRMKTGR,PRIORMKG    ELSE - SAME MARKET GROUP?                    
         BE    IV0900              YES - SO CONTINUE                            
*                                                                               
IV0850   EQU   *                                                                
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE A PAGE BREAK                           
*                                                                               
         MVC   PRIORMKG,SRMKTGR    SAVE THE NEW MARKET GROUP                    
*                                                                               
IV0900   EQU   *                                                                
*                                                                               
         MVC   PRMKT,SRMKT         MARKET NUMBER                                
         MVC   PRSTA,SRSTA         STATION CALL LETTERS                         
         MVC   PRCLT,SRCLT         CLIENT                                       
         MVC   PRPRD,SRPRD         PRODUCT                                      
         MVC   PRPRD2,SRPRD2       PIGGY PRODUCT                                
         MVC   PREST,SREST         ESTIMATE                                     
         GOTO1 DATCON,DMCB,(2,SRPAID),(5,PRPAID)                                
*                                                                               
         LA    R7,SRINVLST         A(INVOICE NUMBERS)                           
         ZIC   R2,SRINVNUM         # OF INVOICES TO PRINT                       
         XR    R3,R3               SET R3 TO 0                                  
         CLI   SRINVNUM,5          MORE THAN 5 INVOICES?                        
         BNH   IV1000              NO - SO CONTINUE                             
*                                                                               
         LA    R2,5                ELSE - FORCE TO 5 FOR 1ST PASS               
         IC    R3,SRINVNUM         # OF INVOICES                                
         SR    R3,R2               -5 FOR 2ND PASS                              
*                                                                               
IV1000   EQU   *                                                                
*                                                                               
         LA    R6,PRINV            A(INVOICE PRINT AREA)                        
*                                                                               
IV1050   EQU   *                                                                
*                                                                               
         MVC   0(10,R6),0(R7)      MOVE OUT THE INVOICE NUMBER                  
         LA    R6,L'PRINV(R6)      INC TO NEXT PRINT POSITION                   
         LA    R7,L'SRINVLST(R7)   INC TO NEXT INV NUMBER                       
         BCT   R2,IV1050           AND LOOP BACK                                
*                                                                               
         GOTO1 REPORT              PRINT THE LINE                               
         CH    R3,=H'0'            NEED A 2ND PASS?                             
         BE    IV1100              NO - SO CONTINUE                             
*                                                                               
         SLDL  R2,R3               R2 = R3, R3 = 0                              
         B     IV1000              AND DO 2ND PASS                              
*                                                                               
IV1100   EQU   *                                                                
*                                                                               
         XC    FIRSTREC,FIRSTREC   CLEAR THE 'FIRST RECORD' FLAG                
         B     IV0800              GO GET NEXT SORT RECORD                      
         DROP  R4,R5                                                            
*                                                                               
IVEXIT   EQU   *                                                                
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
* THIS ROUTINE DOES THE HEADHOOK STUFF                                          
*                                                                               
MYHEAD   NTR1                                                                   
         L     R5,ASORTREC         A(CURRENT SORT RECORD)                       
         USING SORTRECD,R5                                                      
         MVC   H1+14(1),SRMED                                                   
         MVC   H2+14(1),PROFILE+11 MARKET GROUP ID                              
         GOTO1 HEXOUT,DMCB,SRMKTGR,H2+15,2,=C'TOG'                              
         XIT1                                                                   
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* SORT FIELDS=(KEY STARTING COL,KEY LENGTH,ASCENDING/DESCENDING)                
* FORMAT=BINARY, I DON'T KNOW WHAT THE 'WORK=' PARAMETER DOES                   
*                                                                               
* SORT CARD FOR DESCENDING COST SORT                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,23,A),FORMAT=BI,WORK=1'                      
*                                                                               
* RECORD TYPE=FIXED/VARIABLE, LENGTH=SRRECLEN                                   
*                                                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=126'                                   
*                                                                               
         DS    0D                  RE-ALIGN (J.I.C)                             
*                                                                               
ASORTREC DS    A                   A(CURRENT SORT RECORD)                       
ELCODE   DS    X                                                                
FIRSTREC DS    X                   'FIRST RECORD' FLAG                          
MYAGY    DS    X                   AGY CODE IN HIGH ORDER NIBBLE                
MYMED    DS    X                   CURRENT MEDIA IN LOW ORDER                   
MYDATE   DS    XL2                 CURRENT DATE                                 
NEWMED   DS    X                   NEXT MEDIA IN LOW ORDER                      
DATA     DS    X                   'DATA FOUND' FLAG                            
MKGFLAG  DS    X                   'PROCESSED A MKGREC' FLAG                    
PRIORMKG DS    XL2                 PRIOR MARKET GROUP READ                      
PROFILE  DS    CL16                PROFILE AREA                                 
SORTREC  DS    CL(SRRECLEN)        SORT RECORD                                  
MKGLIST  DS    XL20000             INDEXED LIST OF MARKET GROUPS                
*                                                                               
* SORT RECORD DSECT                                                             
*                                                                               
SORTRECD DSECT                                                                  
SRMED    DS    CL1                 MEDIA                                        
SRMKTGR  DS    XL2                 MARKET GROUP NUMBER (PWOS)                   
SRMKT    DS    CL4                 MARKET NUMBER                                
SRSTA    DS    CL5                 STATION CALL LETTERS                         
SRCLT    DS    CL3                 CLIENT CODE                                  
SRPRD    DS    CL3                 PRODUCT CODE                                 
SRPRD2   DS    CL3                 PASSIVE PRODUCT CODE                         
SREST    DS    CL3                 ESTIMATE CODE                                
*                                                                               
SRKEYLEN EQU   *-SORTRECD          SORT KEY LENGTH                              
*                                                                               
SRPAID   DS    XL2                 PAID DATE                                    
SRINVNUM DS    XL1                 NUMBER OF INVOICES                           
SRINVLST DS    10CL10              LIST OF UP TO 10 INVOICES                    
*                                                                               
SRRECLEN EQU   *-SORTRECD          SORT RECORD LENGTH                           
*                                                                               
***********************************************************************         
*        PRINT LINE  DSECT                                                      
***********************************************************************         
PRINTD   DSECT                                                                  
PRGRPLIT DS    CL13                LITERAL                                      
         DS    CL1                                                              
PRMKTGRP DS    CL4                 MAKRKET GROUP NUMBER                         
         DS    CL3                                                              
PRNAME1  DS    CL24                BREAK NAME 1                                 
         DS    CL1                                                              
PRNAME2  DS    CL24                BREAK NAME 2                                 
         DS    CL1                                                              
PRNAME3  DS    CL24                BREAK NAME 3                                 
         DS    CL1                                                              
         ORG   PRGRPLIT                                                         
PRMKT    DS    CL4                 MARKET NUMBER                                
         DS    CL1                                                              
PRSTA    DS    CL5                 STATION CALL LETTERS                         
         DS    CL2                                                              
PRCLT    DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PRPRD    DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PRPRD2   DS    CL3                 PIGGY PRODUCT                                
         DS    CL1                                                              
PREST    DS    CL3                 ESTIMATE                                     
         DS    CL3                                                              
PRPAID   DS    CL8                 PAID DATE                                    
         DS    CL3                                                              
PRINV    DS    5CL11               1ST 5 INVS (10 BYTES/1 SPACE EACH)           
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENAPY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ANIVXX    07/28/98'                                      
         END                                                                    
