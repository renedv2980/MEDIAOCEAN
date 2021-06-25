*          DATA SET DELDPURG   AT LEVEL 002 AS OF 05/01/02                      
*PHASE DELDPURG,+0                                                              
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SCANNER                                                                
         TITLE 'DELDPURG - LOAD/DUMP EXTERN TO PURGE DEMO FILES'                
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R5,RELO             SAVE RELOCATION FACTOR                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*****************************************************************               
* INITIALIZE LOGIC                                              *               
* EXTERN LOOKS FOR PARAM=T,MMM/YY CARD WHERE T=MEDIA CODE AND   *               
* MMM/YY IS PURGE DATE.  BOOKS GREATER THAN PURGE DATE ARE KEPT *               
* ON FILE.                                                      *               
*****************************************************************               
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
         ZAP   LINE,=PL2'99'                                                    
         MVC   P+10(22),=C'DEMO BOOK PURGE EXTERN'                              
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(21),P+10                                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R4,APARAMC          POINT R4 AT PARAMETER CARD                   
         CLC   0(80,R4),SPACES     IS IT BLANK                                  
         BE    MISERR              YES-PUT OUT ERROR MESSAGE                    
*                                                                               
         GOTO1 SCANNER,DMCB,(C'C',APARAMC),(2,WORK),0,RR=RELO                   
         CLI   DMCB+4,2            2 PARAMETERS MUST BE PRESENT                 
         BNE   PARMERR                                                          
         LA    R3,WORK                                                          
         CLI   0(R3),1             MEDIA CODE IS 1 LETTER                       
         BNE   PARMERR                                                          
         TM    2(R3),X'40'         TEST FOR ALPHA                               
         BZ    PARMERR                                                          
         MVC   BYTE,12(R3)         SAVE MEDIA CODE                              
         SPACE 1                                                                
DMXINIT1 DS    0H                  VALIDATE PURGE DATE (MMM/YY)                 
         LA    R3,32(R3)                                                        
         CLI   0(R3),0                                                          
         BE    PARMERR                                                          
         CLI   1(R3),0             MUST BE UNDIVIDED FIELD                      
         BNE   PARMERR                                                          
         GOTO1 DATVAL,DMCB,(2,12(R3)),DUB,RR=RELO                               
         OC    DMCB(4),DMCB        TEST FOR VALID DATE                          
         BZ    PARMERR                                                          
*                                                                               
         MVC   DUB+4(2),=C'01'     MAKE DATE FULL 6 BYTE EBCDIC                 
         GOTO1 DATCON,DMCB,(5,TODAY),TODAY,RR=RELO                              
         CLC   DUB(6),TODAY        TEST FILTER LESS THAN TODAY                  
         BNL   DATERR                                                           
         GOTO1 PERVERT,(R1),DUB,TODAY,RR=RELO                                   
         SR    R2,R2               TEST THAT NUMBER OF MONTHS                   
         ICM   R2,3,DMCB+14        BETWEEN 2 IS AT LEAST 13 MOS.                
         CH    R2,=H'13'                                                        
         BL    DATERR              DATE IS SUSPECT                              
*                                                                               
         GOTO1 DATCON,DMCB,DUB,(3,THREE),RR=RELO                                
         MVC   BOOKFILT,THREE      BINARY YYMM                                  
         MVC   MEDIA,BYTE          SET MEDIA FROM SAVED CODE                    
         B     DMXIT                                                            
         SPACE 1                                                                
* ERROR EXITS                                                                   
*                                                                               
MISERR   DS    0H                                                               
         MVC   P+10(30),=CL30'**MISSING PARAM CARD**'                           
         B     ERRINIT                                                          
         SPACE 1                                                                
PARMERR  DS    0H                                                               
         MVC   P+10(30),=CL30'**INVALID PARAM CARD**'                           
         B     ERRINIT                                                          
         SPACE 1                                                                
DATERR   DS    0H                                                               
         MVC   P+10(30),=CL30'**FILTER DATE REJECTED**'                         
         SPACE 1                                                                
ERRINIT  DS    0H                                                               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P+10(40),=CL40'**PURGE OMITTED DUE TO ERROR**'                   
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
**************************************************************                  
* PROCESS RECORD LOGIC                                       *                  
* MEDIA=0 OR BOOKFILT=X'FFFF' DISABLES PURGE.  RECTAB        *                  
* DEFINES RECORD TYPES WITH BOOK FIELDS AND INDICATES        *                  
* WHETHER BOOK IS COMPLEMENTED.  PURGE STOPS AFTER 4 UNIQUE  *                  
* BOOKS HAVE BEEN ENCOUNTERED TO PROTECT AGAINST WIPING OUT  *                  
* FILE.                                                      *                  
**************************************************************                  
         SPACE 1                                                                
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         L     R1,RECIN            UPDATE RECORDS READ COUNT                    
         LA    R1,1(R1)                                                         
         ST    R1,RECIN                                                         
*                                                                               
         CLI   MEDIA,0             CHECK FOR SUPPRESSION OF PURGE               
         BE    DMXRECK             RESULTING FROM PARAM CARD ERROR              
         CLC   BOOKFILT,=X'FFFF'                                                
         BE    DMXRECK                                                          
*                                                                               
         LA    R8,RECTAB           TEST WHETHER RECORD HAS BOOK                 
         LA    R1,RECORDS          FIELD IN KEY                                 
         CLC   0(1,R3),0(R8)                                                    
         BE    DMXREC2             YES                                          
         LA    R8,L'RECTAB(R8)                                                  
         BCT   R1,*-14                                                          
         B     DMXRECK             NO-KEEP RECORD                               
         SPACE 1                                                                
DMXREC2  DS    0H                                                               
         CLI   0(R3),C'M'          SPECIAL CODE FOR OBSOLETE                    
         BNE   *+12                                                             
         USING BSKEY,R3                                                         
         CLI   BSIND,X'01'         TEST FOR NETWORK MARKET IND                  
         BE    DMXRECM             SPECIAL HANDLING                             
         DROP  R3                                                               
         SPACE 1                                                                
         LR    R4,R3               A(RECORD)                                    
         ZIC   R1,1(R8)            OFFSET OF MEDIA FIELD                        
         AR    R4,R1                                                            
         MVC   BYTE,0(R4)          EXTRACT MEDIA FROM KEY                       
         CLC   BYTE,MEDIA          TEST IF MATCHES PURGE MEDIA                  
         BNE   DMXRECK             NO-KEEP RECORD                               
*                                                                               
         LR    R4,R3               RECORD START                                 
         ZIC   R1,3(R8)            OFFSET OF BOOK                               
         AR    R4,R1                                                            
         MVC   HALF,0(R4)          EXTRACT BOOK FROM KEY                        
         TM    2(R8),X'80'         TEST IF COMPLEMENTED ON KEY                  
         BZ    *+10                                                             
         XC    HALF,=X'FFFF'       YES                                          
*                                  LOOK FOR ABERRENT VALUES IN BOOK FLD         
         CLI   HALF,70             CHECK YEAR                                   
         BL    DMXRECB                                                          
         CLI   HALF,99             COMPARE AGAINST MAXIMUM YEAR                 
         BH    DMXRECB                                                          
         CLI   HALF+1,1            TEST FOR VALID MONTH                         
         BL    DMXRECB                                                          
         CLI   HALF+1,12                                                        
         BH    DMXRECB                                                          
*                                                                               
         CLC   HALF,BOOKFILT       BOOK COMPARED TO PURGE BOOK                  
         BH    DMXRECK                                                          
         SPACE 1                                                                
         LA    R1,4                NO MORE THAN 4 SWEEPS SHOULD BE              
         LA    R2,BOOKTAB          PURGED IN THIS RUN                           
*                                                                               
DMXREC3  OC    0(2,R2),0(R2)       TEST FOR END OF LIST                         
         BZ    DMXREC4                                                          
         CLC   HALF,0(R2)          TEST IF ALREADY IN TABLE                     
         BE    DMXRECP             GO AHEAD AND PURGE RECORD                    
         LA    R2,2(R2)                                                         
         BCT   R1,DMXREC3                                                       
*                                                                               
         MVC   P+10(40),=CL40'**WARNING** MAX OF 4 SWEEPS DELETED'              
         GOTO1 VPRINTER                                                         
         MVC   P+10(33),=C'PURGE HAS STOPPED AT INPUT RECORD'                   
         L     R2,RECIN                                                         
         EDIT  (R2),(10,P+45)                                                   
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   WORK,SPACES         PASS HEADER MESSAGE TO DUMP RTN              
         MVC   WORK+1(3),=C'GET'                                                
         MVI   WORK,3                                                           
         BAS   RE,DUMPREC                                                       
         SPACE 1                                                                
         MVI   MEDIA,0             DISABLE ANY FURTHER PURGING                  
         MVC   BOOKFILT,=X'FFFF'                                                
         B     DMXRECK                                                          
         SPACE 1                                                                
DMXREC4  DS    0H                                                               
         MVC   0(L'HALF,R2),HALF   ADD NEW ENTRY TO PURGE BOOK TABLE            
         B     DMXRECP                                                          
         SPACE 1                                                                
* EXIT POINTS FROM PROCESS RECORD                                               
*                                                                               
DMXRECP  DS    0H                  PURGE RECORD                                 
         ICM   R2,15,4(R8)                                                      
         LA    R2,1(R2)            INCREMENT PURGE COUNT                        
         STCM  R2,15,4(R8)                                                      
         B     DMXPURGE                                                         
         SPACE 1                                                                
DMXRECK  DS    0H                  KEEP RECORD                                  
         L     R1,RECOUT                                                        
         LA    R1,1(R1)            INCREMENT COUNT                              
         ST    R1,RECOUT                                                        
         B     DMXKEEP                                                          
         SPACE 1                                                                
DMXRECB  DS    0H                  BAD BOOK FIELD IN KEY                        
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(22),=CL22'BAD BOOK FIELD IN KEY'                          
         MVI   WORK,22                                                          
         BAS   RE,DUMPREC          DUMP OFFENDING RECORD                        
         L     R1,RECOUT           BUT KEEP IT ON FILE                          
         LA    R1,1(R1)                                                         
         ST    R1,RECOUT                                                        
         L     R1,BADREC           UPDATE BAD RECORD COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,BADREC                                                        
         B     DMXKEEP                                                          
         SPACE 1                                                                
DMXRECM  DS    0H                  OLD MARKET RECORD CODE                       
         L     R1,OLDMREC                                                       
         LA    R1,1(R1)            UPDATE OLD MARKET RECORD                     
         ST    R1,OLDMREC          COUNT                                        
         SR    R0,R0                                                            
         D     R0,=F'25'           DUMP EVERY 25 RECORDS                        
         LTR   R0,R0                                                            
         BNZ   DMXRECM2                                                         
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(2),=C'M1'                                                 
         MVI   WORK,2                                                           
         BAS   RE,DUMPREC                                                       
         SPACE 1                                                                
DMXRECM2 L     R1,RECOUT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECOUT                                                        
         L     R1,BADREC           UPDATE BAD RECORD COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,BADREC                                                        
         B     DMXKEEP                                                          
         EJECT                                                                  
***************************************************************                 
* END-OF-FILE LOGIC                                           *                 
* SHOW TOTALS FOR RECORDS IN, OUT, AND PURGED. THEN GIVE      *                 
* RECORD TYPES FOR WHICH PURGES WERE DONE AND BOOKS PURGED.   *                 
***************************************************************                 
         SPACE 1                                                                
DMXEOF   DS    0H                                                               
         MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         MVC   P+10(26),=C'END OF FILE SUMMARY TOTALS'                          
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(25),P+10                                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,BUCKETS          PRINT THE BUCKETS AND DESCRIPTIONS           
         LA    R3,BUCKTAB          IN A LOOP                                    
DMXEOFA  MVC   P+10(20),4(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            BUCKET VALUE                                 
         EDIT  (R2),(10,P+32)                                                   
         GOTO1 VPRINTER                                                         
         LA    R3,L'BUCKTAB(R3)    POINT TO NEXT BUCKET                         
         BCT   R4,DMXEOFA                                                       
*                                                                               
         SR    R2,R2               CLEAR ACCUMULATOR                            
         LA    R8,RECTAB                                                        
         LA    R0,RECORDS          COUNTER                                      
         MVC   FULL,4(R8)          COUNT OF DELETED FOR TYPE                    
         A     R2,FULL             UPDATED SUM OF DELETIONS                     
         LA    R8,L'RECTAB(R8)                                                  
         BCT   R0,*-14                                                          
         SPACE 1                                                                
DMXEOF1  DS    0H                                                               
         LTR   R2,R2               TEST IF ANY DELETED                          
         BZ    DMXEOFN             NO-EXIT IMMEDIATELY                          
         MVC   P+10(14),=C'RECORDS PURGED'                                      
         MVI   P+30,C'='                                                        
         EDIT  (R2),(10,P+32)                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+10(19),=C'RECORD TYPES PURGED'                                 
         MVI   P+30,C'='                                                        
         LA    R8,RECTAB           R8 POINTS TO TABLE                           
         LA    R0,RECORDS          TABLE ENTRIES                                
         LA    R6,P+32             R6 POINTS TO OUTPUT                          
         SPACE 1                                                                
DMXEOF2  OC    4(4,R8),4(R8)       ANY DELETIONS FOR TYPE                       
         BZ    *+14                                                             
         MVC   0(1,R6),0(R8)       YES-MOVE TYPE TO OUTPUT                      
         LA    R6,2(R6)            BUMP OUTPUT POINTER                          
         LA    R8,L'RECTAB(R8)                                                  
         BCT   R0,DMXEOF2                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+10(12),=C'BOOKS PURGED'                                        
         MVI   P+30,C'='                                                        
*                                                                               
         LA    R8,BOOKTAB          R8 POINTS TO TABLE                           
         LA    R2,4                MAXIMUM ENTRIES IN TABLE                     
         LA    R6,P+32             R6 POINTS TO OUTPUT                          
         SPACE 1                                                                
DMXEOF3  DS    0H                                                               
         OC    0(2,R8),0(R8)       TEST FOR END OF LIST                         
         BZ    DMXEOF4                                                          
         MVC   THREE(2),0(R8)      EXTRACT TABLE ENTRY                          
         MVI   THREE+2,X'01'                                                    
         GOTO1 DATCON,DMCB,(3,THREE),(6,(R6)),RR=RELO                           
         LA    R6,7(R6)            BUMP OUTPUT POINTER AHEAD                    
         LA    R8,2(R8)            POINT TO NEXT BOOK                           
         BCT   R2,DMXEOF3                                                       
         SPACE 1                                                                
DMXEOF4  DS    0H                                                               
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
DMXEOFN  DS    0H                                                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* DUMPREC EXPECTS HEADER MESSAGE LENGTH IN WORK AND TEXT AT WORK+1              
*                                                                               
DUMPREC  NTR1                                                                   
         MVC   HALF,20(R3)         EXTRACT RECORD LENGTH                        
         LH    R5,HALF                                                          
         TM    2(R8),X'40'         TEST FOR PASSIVE RECORD                      
         BZ    *+8                                                              
         LA    R5,23               LENGTH OF PASSIVE RECORD                     
         ZIC   R2,WORK             HEADER MESSAGE LENGTH                        
         GOTO1 PRNTBL,DMCB,((R2),WORK+1),(R3),C'DUMP',(R5),=C'2D',     X        
               RR=RELO                                                          
         B     DMXIT                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* RECTAB HAS THE FOLLOWING LAYOUT                                               
*        BYTE 0=RECORD CODE                                                     
*        BYTE 1=OFFSET OF MEDIA IN KEY                                          
*        BYTE 2=X'80' IF BOOK IS COMPLEMENTED IN KEY                            
*               X'40' IF RECORD IS PASSIVE POINTER                              
*        BYTE 3=OFFSET OF BOOK IN KEY                                           
*        BYTES 4-7=COUNT OF PURGED RECORDS                                      
*                                                                               
RECTAB   DS    0CL8                                                             
         DC    C'M',AL1(BSMEDIA-BSKEY),X'C0',AL1(BSBOOK-BSKEY),XL4'0'           
         DC    C'P',AL1(PRMEDIA-PRKEY),X'00',AL1(PRBOOK-PRKEY),XL4'0'           
         DC    C'R',AL1(DRMEDIA-DRKEY),X'80',AL1(DRBOOK-DRKEY),XL4'0'           
         DC    C'S',AL1(SBMEDIA-SBKEY),X'40',AL1(SBBOOK-SBKEY),XL4'0'           
         DC    C'U',AL1(UMEDIA-UKEY),X'C0',AL1(UBOOK-UKEY),XL4'0'               
RECORDS  EQU   (*-RECTAB)/L'RECTAB                                              
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
BOOKFILT DC    X'FFFF'                                                          
MEDIA    DC    X'00'                                                            
BOOKTAB  DC    4XL2'0'                                                          
         EJECT                                                                  
* BUCKET TABLES                                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
RECIN    DC    F'0',CL20'RECORDS IN'                                            
RECOUT   DC    F'0',CL20'RECORDS OUT'                                           
OLDMREC  DC    F'0',CL20'MARKET 1 RECORDS'                                      
BADREC   DC    F'0',CL20'BAD RECORDS'                                           
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* ROUTINE ADDRESSES                                                             
*                                                                               
DATCON   DC    V(DATCON)                                                        
DATVAL   DC    V(DATVAL)                                                        
PERVERT  DC    V(PERVERT)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
SCANNER  DC    V(SCANNER)                                                       
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
THREE    DS    CL3                                                              
FULL     DS    F                                                                
TODAY    DS    CL6                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DELDPURG  05/01/02'                                      
         END                                                                    
