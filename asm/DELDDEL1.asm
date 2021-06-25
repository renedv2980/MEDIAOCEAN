*          DATA SET DELDDEL1   AT LEVEL 039 AS OF 05/01/02                      
*PHASE DELDDEL1,+0                                                              
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
         TITLE 'DELDDEL - SPECIAL WTP/CABLE/XSPILL BEFORE 1995'                 
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
*****************************************************************               
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
**************************************************************                  
* PROCESS RECORD LOGIC                                       *                  
* MEDIA=T AND BOOKS SPECIFIED IN HARD COMPARES.  RECTAB      *                  
* DEFINES RECORD TYPES WITH BOOK FIELDS AND INDICATES        *                  
* WHETHER BOOK IS COMPLEMENTED.  LIMIT OF 5 BOOKS!!!         *                  
**************************************************************                  
         SPACE 1                                                                
DMXREC   DS    0H                                                               
         BC    0,DMXREC0                                                        
         OI    *-3,X'F0'                                                        
* THIS CODE IS FIRST TIME ONLY - NOT AT INIT                                    
         L     R4,VLDDEFN          POINT TO LOAD DEFINITION                     
         USING LDDEFND,R4                                                       
         L     R5,LDDDTFDA         POINT TO DIRECT ACCESS DTF                   
         MVC   FILNAME,22(R5)      FILE NAME IS AT DTF+22                       
         MVC   SUBFILE,28(R5)      EXTRACT SUB-FILE CODE (IE A OR N)            
         DROP  R4                                                               
*                                                                               
DMXREC0  L     R3,AREC             POINT TO RECORD                              
*        L     R1,RECIN            UPDATE RECORDS READ COUNT                    
*        LA    R1,1(R1)                                                         
*        ST    R1,RECIN                                                         
         AP    RECIN,=PL1'1'                                                    
*                                                                               
         CLC   FILNAME(6),=C'DEMFIL'                                            
         BNE   DMXREC1                                                          
         CLC   SUBFILE,2(R3)       CHECK RECORD'S SOURCE AGAINST FILE           
         B     DMXREC1                                                          
*                                                                               
         MVC   WORK,SPACES         DUMP RECORD FIRST                            
         MVC   WORK+1(16),=C'RECORD SOURCE IS'                                  
         MVC   WORK+18(1),2(R3)                                                 
         MVI   WORK,18                                                          
         BAS   RE,DUMPREC                                                       
*                                                                               
         MVC   WORK,SPACES         SEND MESSAGE TO OPERATOR'S CONSOLE           
         MVC   WORK(32),=C'**WARNING** WRONG INPUT TAPE FOR'                    
         MVC   WORK+33(7),FILNAME                                               
         GOTO1 LOGIO,DMCB,1,(41,WORK),RR=RELO                                   
         DC    H'0'                NOW BLOW UP                                  
         SPACE 1                                                                
DMXREC1  DS    0H                                                               
         LA    R8,RECTAB           TEST WHETHER RECORD HAS BOOK                 
         LA    R1,RECORDS          FIELD IN KEY                                 
         CLC   0(1,R3),0(R8)                                                    
         BE    DMXREC2             YES                                          
         LA    R8,L'RECTAB(R8)                                                  
         BCT   R1,*-14                                                          
         B     DMXRECK             NO-KEEP RECORD                               
         SPACE 1                                                                
DMXREC2  DS    0H                                                               
         CLI   0(R3),C'U'          KEEP THE UNIV PASSIVES                       
         BE    DMXRECK                                                          
         CLI   2(R3),C'N'          MUST BE NSI                                  
         BNE   DMXRECK                                                          
         MVC   FILRTYP,0(R3)                                                    
         CLI   0(R3),C'M'          SPECIAL CODE FOR OBSOLETE RECORD             
         BNE   *+12                                                             
         USING BSKEY,R3                                                         
         CLI   BSIND,X'01'         TEST FOR NETWORK MARKET IND                  
         BE    DMXRECB             SPECIAL HANDLING                             
         DROP  R3                                                               
         SPACE 1                                                                
         LR    R4,R3               A(RECORD)                                    
         ZIC   R1,1(R8)            OFFSET OF MEDIA FIELD                        
         AR    R4,R1                                                            
         MVC   BYTE,0(R4)          EXTRACT MEDIA FROM KEY                       
         MVC   FILMEDIA,0(R4)                                                   
*        CLC   BYTE,MEDIA          TEST IF MATCHES PURGE MEDIA                  
         MVI   FILBTYPE,0                                                       
         CLI   BYTE,C'W'                                                        
         BE    WTPDEL              NO-KEEP RECORD                               
         CLI   BYTE,C'T'                                                        
         BNE   DMXRECK             NO-KEEP RECORD                               
         LR    R4,R3               A(RECORD)                                    
         ZIC   R1,8(R8)            OFFSET OF BTYP FIELD                         
         AR    R4,R1                                                            
         MVC   BYTE,0(R4)          EXTRACT BTYP FROM KEY                        
         MVC   FILBTYPE,BYTE                                                    
         CLI   FILBTYPE,X'E0'                                                   
         BNE   *+8                                                              
         MVI   FILBTYPE,C'X'                                                    
*        CLC   BYTE,MEDIA          TEST IF MATCHES PURGE MEDIA                  
         CLI   BYTE,X'E0'          EXTRA SPILL                                  
         BE    *+8                 YES-DELETE                                   
         CLI   BYTE,C'C'           CABLE                                        
         BNE   DMXRECK             NO-KEEP RECORD                               
*                                                                               
WTPDEL   LR    R4,R3               RECORD START                                 
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
*        CLI   HALF+1,1            TEST FOR VALID MONTH                         
*        BL    DMXRECB                                                          
*        CLI   HALF+1,53                                                        
*        BH    DMXRECB                                                          
*                                                                               
         CLC   HALF,=X'5E01'       BOOK VS. PURGE BOOK                          
         BL    DMXREC2A                                                         
         B     DMXRECK             KEEP RECORD OTHERWISE                        
         SPACE 1                                                                
DMXREC2A LA    R1,BOOKS            NO MORE THAN 5 SWEEPS SHOULD BE              
         LA    R2,BOOKTAB          PURGED IN THIS RUN                           
         MVC   FILBOOK(2),HALF                                                  
*                                                                               
DMXREC3  OC    0(5,R2),0(R2)       TEST FOR END OF LIST                         
         BZ    DMXREC4                                                          
         CLC   FILKEY,0(R2)        TEST IF ALREADY IN TABLE                     
         BE    DMXREC4             ADD UP THE RECORD COUNT                      
         LA    R2,L'BOOKTAB(R2)                                                 
         BCT   R1,DMXREC3                                                       
*                                                                               
         MVC   P+10(40),=CL40'**WARNING** MAX OF 5 SWEEPS DELETED'              
         GOTO1 VPRINTER                                                         
         B     DMXRECK                                                          
         SPACE 1                                                                
DMXREC4  DS    0H                                                               
         ICM   R1,15,5(R2)         AND COUNT RECORDS                            
         LA    R1,1(R1)                                                         
         STCM  R1,15,5(R2)                                                      
         CLC   0(5,R2),FILKEY                                                   
         BE    DMXRECP                                                          
         MVC   0(5,R2),FILKEY      ADD NEW ENTRY TO PURGE BOOK TABLE            
*        MVC   P(18),0(R3)                                                      
*        MVC   P+20(5),FILKEY                                                   
*        GOTO1 VPRINTER                                                         
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
*        L     R1,RECOUT                                                        
*        LA    R1,1(R1)            INCREMENT COUNT                              
*        ST    R1,RECOUT                                                        
         AP    RECOUT,=PL1'1'                                                   
         B     DMXKEEP                                                          
         SPACE 1                                                                
DMXRECB  DS    0H                  BAD BOOK FIELD IN KEY                        
*        L     R1,RECOUT           BUT KEEP IT ON FILE                          
*        LA    R1,1(R1)                                                         
*        ST    R1,RECOUT                                                        
         AP    RECOUT,=PL1'1'                                                   
*        L     R1,BADREC                                                        
*        LA    R1,1(R1)            UPDATE BAD KEY COUNT                         
*        ST    R1,BADREC                                                        
         AP    BADREC,=PL1'1'                                                   
         B     DMXKEEP                                                          
         SPACE 1                                                                
         EJECT                                                                  
***************************************************************                 
* END-OF-FILE LOGIC                                           *                 
* SHOW TOTALS FOR RECORDS IN, OUT, AND PURGED. THEN GIVE      *                 
* RECORD TYPES FOR WHICH PURGES WERE DONE AND BOOKS PURGED.   *                 
***************************************************************                 
         SPACE 1                                                                
DMXEOF   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+10(7),FILNAME                                                  
         MVC   P+18(14),=C'SUMMARY TOTALS'                                      
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(21),P+10                                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,BUCKETS                                                       
         LA    R3,BUCKTAB                                                       
DMXEOFA  MVC   P+10(20),8(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            VALUE                                        
         EDIT  (P8,0(R3)),(12,P+32)                                             
         GOTO1 VPRINTER                                                         
         LA    R3,L'BUCKTAB(R3)    POINT TO NEXT BUCKET                         
         BCT   R4,DMXEOFA                                                       
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
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
         EDIT  (R2),(12,P+32)                                                   
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
         LA    R2,BOOKS            MAXIMUM ENTRIES IN TABLE                     
         LA    R6,P+32             R6 POINTS TO OUTPUT                          
         SPACE 1                                                                
DMXEOF3  DS    0H                                                               
         OC    0(2,R8),0(R8)       TEST FOR END OF LIST                         
         BZ    DMXEOF4                                                          
         MVC   0(3,R6),0(R8)                                                    
         MVC   THREE(2),2(R8)      EXTRACT TABLE ENTRY                          
         MVI   THREE+2,X'01'                                                    
         EDIT  (B1,3(R8)),(2,4(R6))                                             
         EDIT  (B1,4(R8)),(2,6(R6))                                             
         EDIT  (B4,5(R8)),(10,9(R6))                                            
*        GOTO1 DATCON,DMCB,(3,THREE),(6,(R6)),RR=RELO                           
*        LA    R6,7(R6)            BUMP OUTPUT POINTER AHEAD                    
         GOTO1 VPRINTER                                                         
         LA    R8,L'BOOKTAB(R8)     POINT TO NEXT BOOK                          
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
         CLI   HALF,X'FF'          TEST FOR PASSIVE RECORD                      
         BNE   *+8                                                              
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
RECTAB   DS    0CL9                                                             
         DC    C'M',AL1(BSMEDIA-BSKEY),X'C0',AL1(BSBOOK-BSKEY),XL4'0'           
         DC    AL1(BSBTYP-BSKEY)                                                
         DC    C'P',AL1(PRMEDIA-PRKEY),X'00',AL1(PRBOOK-PRKEY),XL4'0'           
         DC    AL1(PRBTYP-PRKEY)                                                
         DC    C'D',AL1(DRMEDIA-DRKEY),X'80',AL1(DRBOOK-DRKEY),XL4'0'           
         DC    AL1(DRBTYP-DRKEY)                                                
         DC    C'R',AL1(DRMEDIA-DRKEY),X'80',AL1(DRBOOK-DRKEY),XL4'0'           
         DC    AL1(DRBTYP-DRKEY)                                                
         DC    C'S',AL1(SBMEDIA-SBKEY),X'40',AL1(SBBOOK-SBKEY),XL4'0'           
         DC    AL1(SBBTYP-SBKEY)                                                
         DC    C'U',AL1(UMEDIA-UKEY),X'C0',AL1(UBOOK-UKEY),XL4'0'               
         DC    AL1(UBTYP-UKEY)                                                  
RECORDS  EQU   (*-RECTAB)/L'RECTAB                                              
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
MEDIA    DC    C'T'                                                             
FILNAME  DC    CL7' '                                                           
SUBFILE  DC    C' '                                                             
         SPACE 2                                                                
* BUCKET TABLE                                                                  
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL28                                                            
RECIN    DC    PL8'0',CL20'RECORDS IN'                                          
RECOUT   DC    PL8'0',CL20'RECORDS OUT'                                         
BADREC   DC    PL8'0',CL20'BAD RECORDS'                                         
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* ROUTINE ADDRESSES                                                             
*                                                                               
DATCON   DC    V(DATCON)                                                        
PRNTBL   DC    V(PRNTBL)                                                        
LOGIO    DC    V(LOGIO)                                                         
         SPACE 2                                                                
BOOKTAB  DC    4000XL9'0'                                                       
BOOKS    EQU   (*-BOOKTAB)/L'BOOKTAB                                            
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
*                                                                               
FILKEY   DS    0CL5                                                             
FILRTYP  DS    C                                                                
FILMEDIA DS    C                                                                
FILBTYPE DS    C                                                                
FILBOOK  DS    CL2                                                              
*                                                                               
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
**PAN#1  DC    CL21'039DELDDEL1  05/01/02'                                      
         END                                                                    
