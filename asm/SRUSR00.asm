*          DATA SET SRUSR00    AT LEVEL 002 AS OF 07/09/14                      
*PHASE T16800A                                                                  
*INCLUDE GETIDS                                                                 
         TITLE '$USER - DISPLAY COMPATIBLE USER ID DATA'                        
         PRINT NOGEN                                                            
USER     CSECT                                                                  
         NMOD1 WRKX-WRKD,**$USER*,RR=RE                                         
         USING WRKD,RC                                                          
         MVC   SRPARS,0(R1)        SAVE CALLING PARAMS                          
         ST    RE,RELO                                                          
         L     R3,SRPAR6           R3=A(TWA)                                    
         USING SRUSRFFD,R3                                                      
         L     R4,SRPAR3           R4=A(UTL)                                    
         USING UTLD,R4                                                          
         L     RA,SRPAR1           RA=A(SYSFAC)                                 
         USING SYSFACD,RA                                                       
         L     RF,SRPAR4           EXTRACT ANY COMFACS ROUTINES NEEDED          
         USING COMFACSD,RF                                                      
         MVC   VXSORT,CXSORT                                                    
         DROP  RF                                                               
*                                                                               
         L     RE,VSSB             EXTRACT SSB DATA                             
         MVC   SYSNAME,SSBSYSN4-SSBD(RE)                                        
         L     RE,SRPAR8                                                        
         MVC   PFK,TIOBAID-TIOBD(RE)                                            
*                                                                               
         XC    DMCB(12),DMCB       GET A(GETIDS) CORE RES PHASE                 
*&&UK*&& MVC   DMCB+4(4),=X'D9000AF9'                                           
*&&US*&& MVC   DMCB+4(4),=X'D9000AFA'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   CGETIDS,0(R1)       CORE RESIDENT PHASE VERSION                  
         MVC   VGETIDS,CGETIDS                                                  
         MVC   AGETIDS,CGETIDS                                                  
         ICM   RF,15,=V(GETIDS)                                                 
         BZ    USER1                                                            
         A     RF,RELO                                                          
         ST    RF,VGETIDS          LINKED IN VERSION FOR TESTING                
*                                                                               
USER1    XC    USERDATA,USERDATA   SET NO USER ID INPUT                         
         MVC   USERID,TUSER                                                     
         MVI   FLAG1,0                                                          
         MVI   FLAG2,0                                                          
         XC    STRNUM,STRNUM                                                    
         L     RE,=A(UIDLST-WRKD)  SET A(USERID LIST FOR GETIDS)                
         AR    RE,RC                                                            
         ST    RE,AUIDLST                                                       
         XC    NUIDLST,NUIDLST     SET NUM OF ENTRIES IN USERID LIST            
         LA    RE,USERDATA                                                      
         ST    RE,AUIDALP                                                       
         XC    LASTDATA,LASTDATA                                                
*                                                                               
         MVI   DDS,0               INITIALISE TERMINAL FLAG                     
         TM    TSTAT,X'60'                                                      
         BZ    *+12                                                             
         MVI   DDS,X'01'           SET DDS TERMINAL                             
         B     USER2                                                            
*                                                                               
         LA    R2,SRVIDH           SET R2 IN CASE OF ERROR                      
         TM    TSTAT7,TST7PQPU                                                  
         BZ    ERR3                                                             
         OI    DDS,X'02'           SET PRIVILEGED USER                          
         OC    TUSER,TUSER                                                      
         BZ    ERR4                MUST BE LOGGED ON                            
USER2    EQU   *                                                                
         EJECT                                                                  
SRVAL    LA    R2,SRVIDH           LOOK FOR =USER,USERID                        
         CLI   SRVID+5,C','                                                     
         BNE   SRVALX                                                           
         LA    RF,SRVID+6                                                       
         CLI   SRVID+6,C'N'                                                     
         BNE   SRVAL0                                                           
         CLI   SRVID+7,0           =USER,N FOR NEW LINKED VERSION               
         BNE   *+14                                                             
         MVC   AGETIDS,VGETIDS                                                  
         B     SRVALX                                                           
         CLI   SRVID+7,C','        =USER,N,USERID IS ALSO ALLOWED               
         BNE   *+14                                                             
         MVC   AGETIDS,VGETIDS                                                  
         LA    RF,2(RF)                                                         
SRVAL0   LA    R0,8                                                             
         LR    R1,RF               SAVE START OF USERID FILED                   
SRVAL1   CLI   0(RF),C' '                                                       
         BNH   SRVAL2                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,SRVAL1                                                        
         B     ERR1                                                             
SRVAL2   LR    RE,R1               POINT TO START OF USERID FIELD               
         SR    RF,RE                                                            
         BZ    SRVALX                                                           
         MVC   USERALP,SPACES                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   USERALP(0),0(RE)                                                 
         TM    DDS,X'01'           ONLY DDS TERMS CAN ENTER A USER ID           
         BZ    ERR2                                                             
SRVALX   EQU   *                                                                
*                                                                               
P1VAL    LA    R2,SRVP1H           P1 - USER ID                                 
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    P1VALX                                                           
         OC    USERALP,USERALP                                                  
         BNZ   ERR2                ALREADY DEFINED IN S/R FIELD                 
         MVC   USERALP,SPACES                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   USERALP(0),8(R2)                                                 
         TM    DDS,X'01'           ONLY DDS TERMS CAN ENTER A USER ID           
         BZ    ERR2                                                             
P1VALX   EQU   *                                                                
*                                                                               
P2VAL    LA    R2,SRVP2H           P2 - VARIOUS OPTIONS                         
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    P2VALX                                                           
         TM    DDS,X'01'           ONLY VALID FOR DDS TERMINALS                 
         BZ    P2VALX                                                           
P2VAL1   CLC   8(6,R2),=C'SORT=N'                                               
         BNE   *+12                                                             
         OI    FLAG1,X'01'                                                      
         B     P2VALX                                                           
P2VAL2   CLC   8(5,R2),=C'USER='                                                
         BNE   P2VALX                                                           
         AHI   R1,-6                                                            
         BNP   P2VALX                                                           
         MVC   TESTUSER,SPACES                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TESTUSER(0),13(R2)                                               
         OI    FLAG1,X'02'                                                      
P2VALX   EQU   *                                                                
*                                                                               
P3VAL    LA    R2,SRVP3H           P3 - START ENTRY NUMBER FOR DISPLAY          
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    P3VALX                                                           
         CHI   R1,8                                                             
         BH    P3VALX                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    P3VALX                                                           
P3VAL1   AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,STRNUM                                                        
         LTR   R0,R0                                                            
         BZ    P3VALX                                                           
         OI    FLAG1,X'04'         SET START DISPLAY NUMBER ENTERED             
P3VALX   EQU   *                                                                
*                                                                               
USER3    OC    USERALP,USERALP     VALIDATE USER ID NAME                        
         BZ    USER4                                                            
         LA    RF,IDREC            READ USER ID NAME RECORD                     
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,USERALP                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(RF),(RF)                            
         CLI   8(R1),0                                                          
         BNE   ERR5                ERROR IF REC NOT FOUND                       
         LA    RE,CTIDATA                                                       
         SR    R1,R1                                                            
USER3A   CLI   0(RE),0             SEARCH FOR DESCRIPTION ELEMENT               
         BE    ERR6                                                             
         CLI   0(RE),X'02'                                                      
         BE    USER3B                                                           
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     USER3A                                                           
USER3B   MVC   USERNUM,2(RE)       EXTRACT USER ID NUMBER                       
         B     USER5                                                            
*                                                                               
USER4    OC    USERID,USERID       MUST HAVE INPUT IF NOT LOGGED ON             
         BNZ   *+12                                                             
         LA    R2,SRVP1H                                                        
         B     ERR1                                                             
         LA    RF,IDREC            READ LOGON USER ID RECORD                    
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKNUM,USERID                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(RF),(RF)                            
         CLI   8(R1),0                                                          
         BNE   ERR5                ERROR IF REC NOT FOUND                       
         LA    RE,CTIDATA                                                       
         SR    R1,R1                                                            
USER4A   CLI   0(RE),0             SEARCH FOR DESCRIPTION ELEMENT               
         BE    ERR6                                                             
         CLI   0(RE),X'02'                                                      
         BE    USER4B                                                           
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     USER3A                                                           
USER4B   MVC   USERNUM,USERID      SET LOGON USER ID NUMBER                     
         MVC   USERALP,SPACES                                                   
         IC    R1,1(RE)                                                         
         AHI   R1,-3                                                            
         BNP   ERR6                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   USERALP(0),2(RE)    EXTRACT USER ID NAME                         
         B     USER5                                                            
*                                                                               
USER5    L     R5,SRPAR2           R5=A(TIA)                                    
         USING SRSD,R5                                                          
         B     USER6               DONT USE TIA ANYMORE AS LIST TOO BIG         
         CLI   FLAG1,0             TEST FOR SPECIAL OPTIONS                     
         BNE   USER5A                                                           
         LA    RF,SRPAGENO         READ S/R SAVE PAGE                           
         SLL   RF,32-8                                                          
         MVC   DMCB+20(2),=C'L='                                                
         LH    R0,=Y(SRSMAX)                                                    
         STH   R0,DMCB+22                                                       
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(RF),SRSD                   
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
USER5A   L     RE,=A(SRUIDLST-SRSD)                                             
         AR    RE,R5                                                            
         ST    RE,AUIDLST                                                       
         XC    NUIDLST,NUIDLST                                                  
         L     RE,=A(SRUIDALP-SRSD)                                             
         AR    RE,R5                                                            
         ST    RE,AUIDALP                                                       
         MVC   LASTDATA,0(RE)      EXTRACT LAST USER ID DATA                    
         TM    DDS,X'01'           TEST DDS TERMINAL                            
         BO    USER6                                                            
         CLI   FLAG1,0             TEST FOR SPECIAL OPTIONS                     
         BNE   USER6                                                            
         CLC   USERNUM,LASTNUM     TEST SAME USER ID                            
         BNE   USER6                                                            
         MVC   USERDATA,LASTDATA                                                
         B     USERA               BYPASS CALLING GETIDS                        
*                                                                               
USER6    TM    FLAG1,X'02'         ARE WE TESTING A USER ID                     
         BO    USERT                                                            
         L     RF,AGETIDS          RF=A(GETIDS)                                 
         LA    RE,IDREC            BUILD GETIDS PARAMETER LIST                  
         ST    RE,DMCB                                                          
         MVI   DMCB,C'L'                                                        
         L     R6,AUIDLST          R6=A(USER ID LIST)                           
         ST    R6,DMCB+4                                                        
         MVC   DMCB+8(4),VDATAMGR                                               
         GOTO1 (RF),DMCB                                                        
*                                                                               
USER7    CLI   0(R6),X'FF'         CHECK FOR ENTRIES WITH MISSING NUMS          
         BE    USER7X                                                           
         LH    R0,NUIDLST          BUMP NUMBER OF ENTRIES                       
         AHI   R0,1                                                             
         STH   R0,NUIDLST                                                       
         OC    10(2,R6),10(R6)     TEST ZERO USER ID NUMBER                     
         BZ    USER7B                                                           
USER7A   LA    R6,12(R6)           BUMP TO NEXT ENTRY IN LIST                   
         B     USER7                                                            
USER7B   LA    RF,IDREC            READ USER ID NAME RECORD                     
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,0(R6)                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(RF),(RF)                            
         CLI   8(R1),0                                                          
         BNE   USER7A              IGNORE IF REC NOT FOUND                      
         LA    RE,CTIDATA                                                       
         SR    R1,R1                                                            
USER7C   CLI   0(RE),0             SEARCH FOR DESCRIPTION ELEMENT               
         BE    USER7A                                                           
         CLI   0(RE),X'02'                                                      
         BE    USER7D                                                           
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     USER7C                                                           
USER7D   MVC   10(2,R6),2(RE)      MOVE USER ID NUMBER TO USERID LIST           
         B     USER7A                                                           
USER7X   EQU   *                                                                
*                                                                               
USER8    TM    FLAG1,X'01'         TEST SORT=NO                                 
         BO    USER8X                                                           
         L     R6,AUIDLST          SORT TABLE IN TO ALPHA USERID ORDER          
         ST    R6,DMCB             A(USERID TABLE)                              
         LH    RE,NUIDLST                                                       
         ST    RE,DMCB+4           NUMBER OF ENTRIES                            
         LA    RE,12                                                            
         ST    RE,DMCB+8           ENTRY LENGTH                                 
         LA    RE,08                                                            
         ST    RE,DMCB+12          KEY LENGTH                                   
         SR    RE,RE                                                            
         ST    RE,DMCB+16          DISPLACEMENT TO START OF KEY                 
         GOTO1 VXSORT,DMCB                                                      
USER8X   EQU   *                                                                
*                                                                               
USER9    CLI   FLAG1,0             TEST SPECIAL OPTIONS INPUT                   
         BNZ   *+8                                                              
         OI    DDS,X'80'           SET TO WRITE BACK TEMPSTR                    
         MVC   USERCNT,NUIDLST                                                  
         L     RE,AUIDALP                                                       
         MVC   0(L'USERDATA,RE),USERDATA                                        
         MVC   LASTDATA,USERDATA                                                
*                                                                               
USERA    LA    R7,SRVLN1H          FIRST LINE                                   
         LA    R8,22               NUMBER OF LINES                              
         MVC   PRVUSER,SPACES                                                   
         LH    R0,USERCNT                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   SRVMSG+33(7),=C' TOTAL='                                         
         NC    SRVMSG+35(4),LOWER                                               
         UNPK  SRVMSG+40(4),DUB                                                 
         L     RE,AUIDLST                                                       
         TM    FLAG1,X'04'         TEST IF START NUMBER INPUT                   
         BZ    USERA1                                                           
         CH    R0,STRNUM           TEST IF HIGHER THAN NUMBER OF ENTRYS         
         BL    USERA1                                                           
         LH    RF,STRNUM                                                        
         AHI   RF,-1                                                            
         MHI   RF,12                                                            
         AR    RE,RF                                                            
*                                                                               
USERA1   LA    RF,8(R7)            DISPLAY 8 IDS PER LINE                       
         LA    R0,8                                                             
USERA2   CLI   0(RE),X'FF'                                                      
         BE    USERAX                                                           
         MVC   0(8,RF),0(RE)                                                    
         CLC   0(8,RF),PRVUSER     TEST DUPLICATE ENTRY                         
         BNE   *+10                                                             
         MVC   0(8,RF),DUPUSER                                                  
         MVC   PRVUSER,0(RE)                                                    
         MVI   9(RF),C' '                                                       
         CLI   8(RE),0             TEST IF NEW STYLE LIST                       
         BNE   USERA3                                                           
         TM    9(RE),X'40'         TEST IF ID IS READ-ONLY                      
         BZ    USERA3                                                           
         CLC   0(8,RF),DUPUSER                                                  
         BE    USERA3                                                           
         NC    0(8,RF),LOWER       CONVERT TO LOWER CASE                        
USERA3   LA    RE,12(RE)                                                        
         LA    RF,9(RF)                                                         
         BCT   R0,USERA2                                                        
         LA    R7,L'SRVLN1+8(R7)                                                
         BCT   R8,USERA1                                                        
*                                                                               
USERAX   CLI   SRVP3H+8,0          POSITION CURSOR TO LAST PARAM                
         BE    *+16                                                             
         NI    SRVIDH+6,X'BF'                                                   
         OI    SRVP4H+6,X'40'                                                   
         B     USERB                                                            
         CLI   SRVP2H+8,0                                                       
         BE    *+16                                                             
         NI    SRVIDH+6,X'BF'                                                   
         OI    SRVP3H+6,X'40'                                                   
         B     USERB                                                            
         CLI   SRVP1H+8,0                                                       
         BE    *+16                                                             
         NI    SRVIDH+6,X'BF'                                                   
         OI    SRVP2H+6,X'40'                                                   
         B     USERB                                                            
*                                                                               
USERB    TM    DDS,X'80'           TEST IF WE CHANGED THE TIA                   
         BZ    EXIT                                                             
         B     EXIT                DONT WRITE BACK TEMPSTR ANYMORE              
         L     RE,=A(SRUIDALP-SRSD)                                             
         AR    RE,R5                                                            
         MVC   0(L'USERDATA,R5),USERDATA                                        
         LA    RF,SRPAGENO         WRITE BACK S/R SAVE PAGE                     
         SLL   RF,32-8                                                          
         MVC   DMCB+20(2),=C'L='                                                
         LH    R0,=Y(SRSMAX)                                                    
         STH   R0,DMCB+22                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),TEMPSTR,(RF),SRSD                    
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         B     EXIT                                                             
*                                                                               
USERT    L     RF,AGETIDS          RF=A(GETIDS)                                 
         LA    RE,IDREC            BUILD GETIDS PARAMETER LIST                  
         ST    RE,DMCB                                                          
         MVI   DMCB,C'C'                                                        
         XC    DMCB+4(4),DMCB+4    R6=A(USER ID LIST)                           
         MVC   DMCB+8(4),VDATAMGR                                               
         MVI   DMCB+8,C'A'         SET TO PASS INPUT USERID TO TEST             
         LA    RE,TESTUSER                                                      
         ST    RE,DMCB+12                                                       
         GOTO1 (RF),DMCB                                                        
*                                                                               
         MVC   SRVMSG,=CL60'USER ID IS NOT COMPATIBLE'                          
         TM    DMCB+12,X'01'                                                    
         BZ    *+14                                                             
         MVC   SRVMSG,=CL60'USER ID IS COMPATIBLE'                              
         B     USERTX                                                           
         TM    DMCB+12,X'02'                                                    
         BZ    USERTX                                                           
         MVC   SRVMSG,=CL60'ALL USER IDS ARE COMPATIBLE'                        
*                                                                               
USERTX   NC    SRVMSG+1(59),LOWER  CONVERT TO LOWER CASE                        
         NI    SRVIDH+6,X'BF'      UNSET CURSOR                                 
         OI    SRVP3H+6,X'40'      INSERT CURSOR AT NEXT FIELD                  
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
ERR1     LA    R1,1                MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    R1,2                INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR3     LA    R1,3                INVALID SERVICE REQUEST                      
         B     ERRX                                                             
ERR4     LA    R1,4                MUST BE LOGGED ON                            
         LA    R2,SRVP1H                                                        
         B     ERRX                                                             
ERR5     LA    R1,5                USERID NOT ON FILE                           
         LA    R2,SRVP1H                                                        
         B     ERRX                                                             
ERR6     LA    R1,6                INVALID USER ID RECORD                       
         LA    R2,SRVP1H                                                        
         B     ERRX                                                             
*                                                                               
ERRX     MVC   SRVMSG(15),=C'ED/9999 (XXXX) '                                   
         MVC   SRVMSG+09(4),SYSNAME                                             
         L     RE,=A(ERRMSGS)                                                   
         A     RE,RELO                                                          
         BCTR  R1,0                                                             
         SLL   R1,5                (ERRNUM-1)*32                                
         AR    RE,R1                                                            
         MVC   SRVMSG+15(32),0(RE)                                              
         NC    SRVMSG+16(31),LOWER CONVERT TO LOWER CASE                        
         NI    SRVIDH+6,X'BF'      UNSET CURSOR                                 
         OI    6(R2),X'40'         INSERT CURSOR AT ERROR                       
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
CTFILE   DC    CL8'CTFILE'                                                      
TEMPSTR  DC    CL8'TEMPSTR'                                                     
DUPUSER  DC    CL8'......  '                                                    
SPACES   DC    CL16' '                                                          
LOWER    DC    60X'BF'                                                          
*                                                                               
ERRMSGS  DS    0CL32                                                            
         DC    CL32'MISSING INPUT FIELD'                                        
         DC    CL32'INVALID INPUT FIELD'                                        
         DC    CL32'INVALID SERVICE REQUEST'                                    
         DC    CL32'MUST BE LOGGED ON'                                          
         DC    CL32'USERID NOT ON FILE'                                         
         DC    CL32'INVALID USER ID RECORD'                                     
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
RELO     DS    F                                                                
CGETIDS  DS    A                   CORE RES VESRION                             
VGETIDS  DS    A                   LINKED VERSION (FOR TESTING)                 
AGETIDS  DS    A                   VERSION USED THIS TIME                       
AUIDALP  DS    A                                                                
AUIDLST  DS    A                                                                
NUIDLST  DS    H                                                                
STRNUM   DS    H                                                                
FLAG1    DS    X                                                                
FLAG2    DS    X                                                                
TESTUSER DS    CL10                                                             
PRVUSER  DS    CL8                                                              
*                                                                               
SRPARS   DS    0CL32                                                            
SRPAR1   DS    A                   A(SYSFACS)                                   
SRPAR2   DS    A                   A(TIA)                                       
SRPAR3   DS    A                   A(UTL)                                       
SRPAR4   DS    A                   A(COMFACS)                                   
SRPAR5   DS    A                   A(SELIST)                                    
SRPAR6   DS    A                   A(TWA)                                       
SRPAR7   DS    A                   A(PHASE MAP)                                 
SRPAR8   DS    A                   A(TIOB)                                      
*                                                                               
VXSORT   DS    A                                                                
SYSNAME  DS    CL4                                                              
*                                                                               
DMCB     DS    CL24                                                             
USERID   DS    XL2                                                              
DDS      DS    X                                                                
PFK      DS    X                                                                
*                                                                               
USERDATA DS    0XL16                                                            
USERALP  DS    CL10                                                             
USERNUM  DS    XL2                                                              
USERCNT  DS    XL2                                                              
USERCNL  DS    XL2                                                              
*                                                                               
LASTDATA DS    0XL16                                                            
LASTALP  DS    CL10                                                             
LASTNUM  DS    XL2                                                              
LASTCNT  DS    XL2                                                              
LASTCNL  DS    XL2                                                              
*                                                                               
IDREC    DS    CL2000                                                           
         ORG   IDREC                                                            
CTIKEY   DS    0CL25     C         KEY                                          
CTIKTYP  DS    CL1       C                                                      
CTIKTYPQ EQU   C'I'                                                             
CTIKSPAR DS    CL14      C         SPARE (BINARY ZEROES)                        
CTIKID   DS    CL10      C         ID NAME                                      
         ORG   CTIKID                                                           
         DS    XL8                 N/D                                          
CTIKNUM  DS    XL2                 ID NUMBER                                    
CTILEN   DS    CL2       X         RECORD LENGTH                                
CTISTAT  DS    CL1       X         STATUS                                       
CTIDATA  DS    0C        V         DATA (X'01' ACTIVITY ELEMENT)                
         ORG                                                                    
IDREC1   DS    CL2000                                                           
*                                                                               
UIDLST   DS    3000XL12            USER ID LIST FOR GETIDS                      
WRKX     EQU   *                                                                
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRUSRFFD DSECT                                                                  
         DS    CL64                                                             
* SRUSRFFD                                                                      
       ++INCLUDE SRUSRFFD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRUSR00   07/09/14'                                      
         END                                                                    
