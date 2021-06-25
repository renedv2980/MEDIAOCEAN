*          DATA SET PRWRI08    AT LEVEL 091 AS OF 07/17/02                      
*PHASE T40508A,* ******************** NOTE 'A' APPENDED TO PHASE NAME           
*INCLUDE GETCOST                                                                
         TITLE 'CHANGE LOG'                                                     
* ROSA BUG  IF FIRST MONTH IN EST BUCKET DOES NOT MATCH THE START BUG01         
*           REQUEST DATE, THE ELEMENT WAS IGNORED                 BUG01         
* ROSA 11/1/90 PG MODIFICATIONS                                   L01           
         TITLE 'T40508 - P&&G AGENCY DATA COLLECTION'                           
T40508   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40508,RA                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     DS    0H                                                               
*********OI    PBQPER,PBQPMN MUST CREATE A MONTHS TABLE                         
*********MVI   PBQPERLO,1                                                       
*********MVI   PBQPERHI,X'FF'                                                   
*********OI    PBQREAD,DIEST      ESTIMATE                                      
*                                                                               
*        LA    R2,PNGMBRH          VALIDATE MULTI-BRAND OPTION                  
*        MVI   MBROPT,C'N'                                        L01           
*        CLI   5(R2),0                                            L01           
*        BE    INIT2                                              L01           
*        MVC   MBROPT,8(R2)                                       L01           
*        CLI   MBROPT,C'Y'                                        L01           
*        BE    INIT2                                              L01           
*        CLI   MBROPT,C'N'                                        L01           
*        BNE   EINV                                               L01           
*                                                                               
INIT2    MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
         LA    R2,PNGTITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         ICM   R3,15,TWAMASTC      POINT TO MASTER CONTROL BLOCK                
         BZ    VRECMSTX            NOT OFF-LINE                                 
         USING MASTD,R3            ESTABLISH MASTER CONTROL BLOCK               
*                                                                               
         LA    R4,MCREMOTE         POINT TO REMOTE CONTROL BLOCK                
         USING REMOTED,R4          ESTABLISH REMOTE CONTROL BLOCK               
*                                                                               
         OC    REMOTKEY,REMOTKEY   SKIP IF NOT A DIRECT REPORT                  
         BZ    VRECMSTX                                                         
*                                                                               
         GOTO1 TWAVPRNT,DMCB,=C'CLOSE'   STOP CURRENT REPORT                    
*                                                                               
         AP    MCREQNO,=P'1'       BUMP REQUEST NUMBER                          
*                                                                               
         L     RF,MCVREMOT         POINT TO DDPRINT'S REMOTEC                   
         MVC   0(L'MCREMOTE,RF),MCREMOTE  REPLACE WITH CURRENT REMOTE           
         LR    R4,RF               SWITCH POINTERS                              
*                                                                               
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
*                                                                               
         B     VRECREQX            BYPASS                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CONWHENH+5     FIELD LENGTH                                 
         BZ    VRECREQX            SHOULDN'T HAPPEN                             
         LA    R1,CONWHEN-1(RF)    FIND REQUESTOR                               
*                                  LOOK FOR FIRST COMMA                         
         CLI   0(R1),C','                                                       
         BE    *+16                                                             
         AHI   R1,-1               NEXT CHARACTER                               
         BCT   RF,*-12                                                          
         B     VRECREQX            NO COMMA -SHOULDN'T HAPPEN                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,CONWHENH+5     ORIGINAL LENGTH                              
         SR    RE,RF               INITIALS LENGTH                              
         BM    VRECREQX            SHOULDN'T HAPPEN                             
         LR    RF,RE                                                            
*                                                                               
         MVC   REMOTJID,SPACES     INIT INITIALS                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   REMOTJID(0),1(R1)   USER'S INITIALS                              
*                                                                               
VRECREQX DS    0H                                                               
*                                                                               
         MVC   REMOTDSC(11),=CL11'PRGTAPE' REPORT TYPE                          
*                                                                               
         CLI   PNGNAMEH+5,0                                                     
         BE    VRECMSTX           SKIP IF NO NAME                               
*                                                                               
         LA    R1,REMOTDSC+3       NEXT AVAILABLE POSITION                      
         MVC   0(8,R1),PNGNAME     REPORT NAME                                  
*                                                                               
VRECMSTX DS    0H                                                               
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
*VALID    CLI   PBQBPRD,X'FF'       TEST PRD=POL REQUEST                        
*         BNE   *+12                NO                                          
*         MVI   PBQBPRD,0           YES-BREAK OUT THE PRODUCTS                  
*         OI    PBQPIND,PBQPOLSP                                                
VALID     DS    0H                                                              
*                                                                               
         MVI   WIDEOPT,C'Y'        SET FOR WIDE REPORT                          
*                                                                               
         MVC   SUBTITLE(12),=C'PERIOD FROM '                                    
         GOTO1 DATCON,DMCB,(3,PBQBST),(6,SUBTITLE+12)                           
         MVC   SUBTITLE+19(3),=C'TO '                                           
         GOTO1 DATCON,DMCB,(3,PBQBEND),(6,SUBTITLE+22)                          
*****                                                                           
**                                                                              
         OC    SUBTITLE,SPACES                                                  
         GOTO1 CENTER,DMCB,SUBTITLE,36                                          
*                                                                               
          B     XIT                                                             
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLOUTPUT     HOOK BEFORE DRIVER OUPUT                     
         BE    PGEST                                                            
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
         OI    DATAIND,DPROFDEF    REQUIRE DEFAULT BILL PROF                    
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'IF4     ',A(IF4)                                             
         DC    CL8'IF5     ',A(IF5)                                             
         DC    CL8'IF6     ',A(IF6)                                             
         DC    CL8'OF4     ',A(OF4)                                             
         DC    CL8'OF5     ',A(OF5)                                             
         DC    CL8'OF6     ',A(OF6)                                             
         DC    CL8'IERR    ',A(IERR)                                            
         DC    CL8'OERR    ',A(OERR)                                            
         DC    CL8'IDOL    ',A(IDOL)                                            
         DC    CL8'ODOL    ',A(ODOL)                                            
         DC    CL8'HDOL    ',A(HDOL)                                            
         DC    CL8'IPGCPER ',A(IPGCPER)                                         
         DC    CL8'IPGACCT ',A(IPGACCT)                                         
         DC    CL8'IPGBRND ',A(IPGBRND)                                         
         DC    CL8'IPGESTIM',A(IPGESTIM)                                        
         DC    CL8'IPGEVNT ',A(IPGEVNT)                                         
         DC    CL8'IPGMBRD ',A(IPGMBRD)                                         
         DC    CL8'IPGNBRD ',A(IPGNBRD)                                         
         DC    CL8'IPGSRST ',A(IPGSRST)                                         
         DC    CL8'IPGACEST',A(IPGACEST)                                        
         DC    CL8'IPGBRSFX',A(IPGBRSFX)                                        
         DC    CL8'ISPACES ',A(ISPACES)                                         
         DC    CL8'ISQN',A(ISQN)                                                
         DC    CL8'ISQNF4',A(ISQNF4)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVI   GLOPTS,0            DOWNLOAD OPTION                              
         CLI   DOWNOPT,C'N'                                                     
         BE    *+16                                                             
         MVI   GLOPTS,C'D'                                                      
         OI    GLDOWNLD,GLDLNOHD+GLDLNOTR+GLDLALPH                              
         OI    GLINDS2,GLDLNOTR+GLDLALPH                                        
*                                                                               
         MVC   GLOPTS+1(1),MBROPT  MULTI-BRAND OPTION                           
*                                                                               
         CLI   RPTSCRN,X'E8'       SET OPTION IF EDIT LIST                      
         BNE   *+8                                                              
         MVI   GLOPTS+1,C'L'                                                    
         OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
******** CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
******** BNE   *+8                                                              
******** MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
IF4      MVC   0(2,R2),=C'F4'                                                   
         MVI   2(R2),C'A'          MAINTENANCE CODE               L01           
         MVC   3(3,R2),CHGPR       CHARGE PERIOD                                
         MVC   CONVAGY,=C'00'      AGENCY                                       
*                                                                               
         MVI   ERRCD,0             INITIALIZE ERROR CODE                        
         LA    R1,BRANDTAB                                                      
         MVC   6(2,R2),=C'00'     SET AGENCY CODE(FROM TABLE)                   
*                                                                               
PG013    CLI   0(R1),0                                                          
         BNE   PG013A                                                           
         OI    ERRCD,ERRBRC                                                     
         MVC   6(2,R2),=C'XX'                                                   
         B     PG013C                                                           
PG013A   CLC   PBAGY,0(R1)                                                      
         BNE   PG013AC                                                          
         MVC   6(2,R2),4(R1)                                                    
         MVC   CONVAGY,4(R1)                                                    
*                                                                               
         CLC   PBAGY,=C'H9'        IF AGENCY SMG                                
         BNE   PG013AB                                                          
         CLC   PBQCLT,=C'PGB'         IF CLIENT PGB                             
         BE    *+10                                                             
         CLC   PBQCLT,=C'PG1'         OR CLIENT PG1                             
         BNE   *+16                                                             
         MVC   6(2,R2),=C'01'            ASSIGN TO AGENCY 01                    
         MVC   CONVAGY,=C'01'                                                   
*                                                                               
PG013AB  DS    0H                                                               
         B     PG013C                                                           
PG013AC  LA    R1,6(R1)                                                         
         B     PG013                                                            
*                                                                               
PG013C   MVC   8(6,R2),ACCT        ACCT                                         
         MVC   14(4,R2),BRAND      BRAND                                        
*                                                                               
*                                  SKIP                                         
         CLC   PBAGY,=C'H9'        IF AGENCY MEDIAVEST                          
         BNE   PG013D1                                                          
         CLC   PBQCLT,=C'PGB'         IF CLIENT PGB                             
         BE    *+10                                                             
         CLC   PBQCLT,=C'PG1'         OR CLIENT PG1                             
         BE    PG013D                                                           
*                                                                               
PG013D1  DS    0H                                                               
*                                                                               
         CLI   NOBRD,C'Y'                                         L01           
         BNE   *+10                                               L01           
         MVC   14(4,R2),BRDCD                                     L01           
*                                                                               
PG013D   DS    0H                                                               
*                                                                               
         MVC   18(4,R2),ESTIM      ESTIMATE                                     
*** PXZ                                                                         
         MVC   0(20,R2),2(R2)                                                   
         MVC   20(2,R2),=C'F4'                                                  
*** PXZ                                                                         
         MVC   22(2,R2),SPACES     FILLER                                       
         GOTO1 DATCON,DMCB,(0,PBQTODAY),(X'20',DUB)  MAKE DATE YYMMDD           
         MVC   24(4,R2),DUB+2      ESTIMATE DATE MMDDYY                         
         MVC   28(2,R2),DUB                                                     
         MVC   30(6,R2),EVNCD      EVENT CODE                                   
         MVC   36(8,R2),SPACES     FILLER                                       
         MVC   44(1,R2),MLTBR      MULTI-BRAND FLAG                             
         B     XIT                                                              
*                                                                               
OF4      DS    0H                                                               
         MVC   WORK(45),0(R2)                                                   
         MVC   0(2,R2),WORK+20                                                  
         MVC   2(20,R2),WORK                                                    
         MVC   22(23,R2),WORK+22                                                
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
IF5      MVC   0(2,R2),=C'F5'                                                   
         MVI   2(R2),C'A'          MAINTENANCE CODE               L01           
         MVC   3(3,R2),CHGPR       CHARGE PERIOD                                
         MVC   6(2,R2),CONVAGY     AGENCY                                       
         MVC   8(6,R2),ACCT        ACCT                                         
         MVC   14(4,R2),BRAND      BRAND                                        
*                                                                               
         CLC   PBAGY,=C'H9'        IF AGENCY MEDIAVEST                          
         BNE   IF513D1                                                          
         CLC   PBQCLT,=C'PGB'         IF CLIENT PGB                             
         BE    *+10                                                             
         CLC   PBQCLT,=C'PG1'         OR CLIENT PG1                             
         BE    IF513D                                                           
*                                                                               
IF513D1  DS    0H                                                               
*                                                                               
         CLI   NOBRD,C'Y'                                         L01           
         BNE   *+10                                               L01           
         MVC   14(4,R2),BRDCD                                     L01           
*                                                                               
IF513D   DS    0H                                                               
*                                                                               
         MVC   18(4,R2),ESTIM      ESTIMATE                                     
***PXZ                                                                          
         MVC   0(20,R2),2(R2)                                                   
         MVC   20(2,R2),=C'F5'                                                  
***PXZ                                                                          
         MVC   22(2,R2),SPACES     FILLER                                       
         B     XIT                                                              
*                                                                               
OF5      DS    0H                                                               
         MVC   WORK(24),0(R2)                                                   
         MVC   0(2,R2),WORK+20                                                  
         MVC   2(20,R2),WORK                                                    
         MVC   22(2,R2),WORK+22                                                 
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
*                                                                               
IF6      MVC   0(2,R2),=C'F6'                                                   
         MVI   2(R2),C'A'          MAINTENANCE CODE               L01           
         MVC   3(3,R2),CHGPR       CHARGE PERIOD                                
         MVC   6(2,R2),CONVAGY     AGENCY                                       
         MVC   8(6,R2),ACCT        ACCT                                         
*                                                                               
         MVC   14(4,R2),BRDCD      BRAND CODE (FROM TABLE)                      
*                                                                               
         CLC   PBAGY,=C'H9'        IF AGENCY MEDIAVEST                          
         BNE   IF613D1                                                          
         CLC   PBQCLT,=C'PGB'         IF CLIENT PGB                             
         BE    *+10                                                             
         CLC   PBQCLT,=C'PG1'         OR CLIENT PG1                             
         BNE   IF613D1                                                          
*                                                                               
         MVC   14(4,R2),BRAND            USE BRAND                              
*                                                                               
IF613D1  DS    0H                                                               
*                                                                               
         MVC   18(4,R2),ESTIM      ESTIMATE                                     
***PXZ                                                                          
         MVC   0(20,R2),2(R2)                                                   
         MVC   20(2,R2),=C'F6'                                                  
***PXZ                                                                          
         MVC   22(2,R2),BRSFX      BRAND SUFFIX                                 
         MVC   24(4,R2),BRAND      BRAND                                        
         B     XIT                                                              
OF6      DS    0H                                                               
         MVC   WORK(24),0(R2)                                                   
         MVC   0(2,R2),WORK+20                                                  
         MVC   2(20,R2),WORK                                                    
         MVC   22(2,R2),WORK+22                                                 
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 2                                                                
CONVAGY  DS    CL2                                                              
         SPACE 2                                                                
IDOL     XC    0(4,R2),0(R2)                                                    
*                                                                               
         ZIC   RE,GLARGS           GET MONTH NUMBER                             
         BCTR  RE,0                                                             
         SLL   RE,1                GET DISPLACEMENT OF MONTH                    
*                                                                               
         LA    R1,REQMNTHS(RE)     POINT TO MONTH TABLE                         
*                                                                               
         OC    0(2,R1),0(R1)       SKIP IF NO MONTH IN TABLE                    
         BZ    IDOLX                                                            
*                                                                               
         SLL   RE,2                MAKE DISP A MULTIPLE OF 8 SEE SLL            
*                                                                               
         LA    RF,ORDERDOL(RE)     ABOVE -- POINT TO DOLL                       
*                                                                               
         CLC   0(2,R1),0(RF)       SKIP IF MONTH DOESN'T MATCH                  
         BNE   IDOLX                                                            
*                                                                               
         OC    2(6,RF),2(RF)       SKIP IF THERE ARE NO DOLLARS                 
         BZ    IDOLX                                                            
*                                                                               
         ZAP   0(6,R2),2(6,RF)     RETURN DOLLARS TO DRIVER                     
*                                                                               
IDOLX    DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
ODOL     DS    0H                                                               
         ZAP   DUB,0(6,R2)                                                      
         OI    DUB+7,15                                                         
         L     R1,GLADTENT                                                      
         ZIC   RF,DROLEN-DROD(R1)                                               
         LR    RE,RF                                                            
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R3),DUB                                                      
         TM    5(R2),X'01'         TEST NEGATIVE                                
         BZ    XIT                                                              
         SH    RE,=H'3'            YES-OVERPUNCH LAST DOLLAR POSITION           
         LA    RE,0(RE,R3)                                                      
         NI    0(RE),X'DF'                                                      
         B     XIT                                                              
*                                                                               
HDOL     MVC   0(7,R3),=C'DOLLARS'                                              
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,1                                                             
         LA    RE,REQMNTHS(RE)                                                  
         MVC   FULL(2),0(RE)                                                    
         MVI   FULL+2,1                                                         
         LA    R5,198(R3)                                                       
         GOTO1 DATCON,DMCB,(3,FULL),(6,(R5))                                    
         B     XIT                                                              
         SPACE 2                                                                
ISPACES  L     R1,GLADTENT                                                      
         ZIC   RE,DRINLEN-DRIND(R1)                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),SPACES                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        SPECIAL SEQUENCE NUMBER FOR STARCOM - AGENCY H9              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ISQNF4   DS    0H                                                               
*                                                                               
         CLC   PBAGY,=C'H9'        SKIP IF NOT STARCOM                          
         BNE   ISQNF4X                                                          
*                                                                               
         CLC   CHGPR,CHGPRSV       CHECK IF KEY HAS CHANGED                     
         BNE   ISQNF4SX                                                         
*                                                                               
         CLI   NOBRD,C'Y'                                                       
         BE    ISQNF410                                                         
*                                                                               
******   CLC   BRAND,BRANDSV                                                    
******   BNE   ISQNF4SX                                                         
         B     ISQNF420                                                         
*                                                                               
ISQNF410 DS    0H                                                               
*                                                                               
******   CLC   BRDCD,BRDCDSV                                                    
******   BNE   ISQNF4SX                                                         
*                                                                               
ISQNF420 DS    0H                                                               
*                                                                               
         CLC   ESTIM,ESTIMSV                                                    
         BE    ISQNF4X                                                          
*                                                                               
ISQNF4SX DS    0H                                                               
*                                                                               
         MVC   CHGPRSV,CHGPR       SAVE KEY FIELDS                              
         MVC   BRANDSV,BRAND                                                    
         MVC   BRDCDSV,BRDCD                                                    
         MVC   ESTIMSV,ESTIM                                                    
*                                                                               
         LH    RF,ISQNSQN          BUMP SEQUENCE NUMBER                         
         AHI   RF,1                ON CHANGE IN MASTER KEY                      
         STH   RF,ISQNSQN                                                       
*                                                                               
ISQNF4X  DS    0H                                                               
         MVC   0(2,R2),ISQNSQN     RETURN SEQUENCE NUMBER                       
         B     XIT                                                              
*                                                                               
ISQNSQN  DC    H'0'                SEQUENCE NUMBER HOLDAREA                     
*                                                                               
ISQN     DS    0H                  RETURN SEQUENCE NUMBER                       
         MVC   0(2,R2),ISQNSQN     RETURN SEQUENCE NUMBER                       
         B     XIT                                                              
*                                                                               
IERR     MVC   0(1,R2),ERRCD                                                    
         B     XIT                                                              
*                                                                               
*        PGEDIT FIELDS                                                          
*                                                                               
IPGCPER  DS    0H                                                               
*                                                                               
         MVC   0(L'BRECHGPR,R2),BRECHGPR     CHARGE PERIOD                      
         B     XIT                                                              
*                                                                               
IPGACCT  DS    0H                                                               
*                                                                               
         MVC   0(L'BREACCT,R2),BREACCT       ACCOUNT NUMBER                     
         B     XIT                                                              
*                                                                               
IPGBRND  DS    0H                                                               
*                                                                               
         MVC   0(L'BREBRAND,R2),BREBRAND     BRAND CODE                         
         B     XIT                                                              
*                                                                               
IPGESTIM DS    0H                                                               
*                                                                               
         MVC   0(L'BREESTIM,R2),BREESTIM     ESTIMATE NUMBER                    
         B     XIT                                                              
*                                                                               
IPGEVNT  DS    0H                                                               
*                                                                               
         MVC   0(L'BREEVNCD,R2),BREEVNCD     EVENT CODE                         
         B     XIT                                                              
*                                                                               
IPGMBRD  DS    0H                                                               
*                                                                               
         MVC   0(L'BREMLTBR,R2),BREMLTBR     MULTI-BRAND INDICATOR              
         B     XIT                                                              
*                                                                               
IPGNBRD  DS    0H                                                               
*                                                                               
         MVC   0(L'BRENOBRD,R2),BRENOBRD     NO BRAND INDICATOR                 
         B     XIT                                                              
*                                                                               
IPGSRST  DS    0H                                                               
*                                                                               
         OC    BREFCLYR,BREFCLYR   SKIP IF NO DATE GIVEN                        
         BZ    IPGSRSTX                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,BREFCLYR),(5,(R2))   SHORT RATE START             
*                                                                               
IPGSRSTX DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
IPGACEST DS    0H                                                               
*                                                                               
         MVC   0(2,R2),BREACEST    ORIGINAL ESTIMATE NUMBER                     
         B     XIT                                                              
*                                                                               
IPGBRSFX DS    0H                                                               
*                                                                               
         MVC   0(2,R2),BREBRSFX    BRAND SUFFIX                                 
         B     XIT                                                              
*                                                                               
OERR     MVC   ERRCD,0(R2)                                                      
         LA    R1,F4ERRMSK                                                      
         CLI   GLARGS,4                                                         
         BE    OERR2                                                            
         LA    R1,F5ERRMSK                                                      
         CLI   GLARGS,5                                                         
         BE    OERR2                                                            
         LA    R1,F6ERRMSK                                                      
         CLI   GLARGS,6                                                         
         BE    OERR2                                                            
         DC    H'0'                                                             
*                                                                               
OERR2    NC    ERRCD,0(R1)                                                      
         LR    R5,R3                                                            
         TM    ERRCD,ERRCHP                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'CHP,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRACC                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'ACC,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRAGY                                       L01           
         BZ    *+14                                               L01           
         MVC   0(4,R5),=C'AGY,'                                   L01           
         LA    R5,4(R5)                                           L01           
         TM    ERRCD,ERRBRN                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'BRN,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERREST                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'EST,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERREVC                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'EVC,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRMBR                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'MBR,'                                                 
         LA    R5,4(R5)                                                         
         TM    ERRCD,ERRBRC                                                     
         BZ    *+14                                                             
         MVC   0(4,R5),=C'BRC,'                                                 
         LA    R5,4(R5)                                                         
         CR    R5,R3                                                            
         BE    XIT                                                              
         BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   XIT                                                              
         MVI   0(R5),C' '                                                       
         B     XIT                                                              
         EJECT                                                                  
* HOOK BEFORE DRIVER OUTPUT - READ PG ESTIMATE RECORDS AND CALL DRIVER          
* FOR INPUT                                                                     
*                                                                               
****** CREATE A TABLE OF BINARY YYMM FOR 6 MONTHS // USED TO LOOKUP             
******             ESTIMATE BUCKET ELEMENTS                                     
*                                                                               
PGEST    XC    REQMNTHS,REQMNTHS   SET REQUEST MONTHS                           
         ZIC   R2,PBQBST         START YEAR                                     
         ZIC   R1,PBQBST+1       START MONTH                                    
         LA    R3,REQMNTHS                                                      
         LA    R0,5                                                             
         MVC   0(2,R3),PBQBST                                                   
PG0      LA    R3,2(R3)                                                         
*                                                                               
         LA    R1,1(R1)    INCREMENT MONTH                                      
         CH    R1,=H'13'                                                        
         BL    PG001                                                            
         LA    R1,1        JAN                                                  
         LA    R2,1(R2) NEW YEAR                                                
PG001    STC   R2,0(R3)                                                         
         STC   R1,1(R3)            MONTH                                        
         BCT   R0,PG0                                                           
*                                                                               
*        FIND PRODUCT AAA IN BUFFER AND SAVE BILLING FORMULA                    
*                                                                               
PGAAA    DS    0H                                                               
*                                                                               
         ICM   R5,15,PBAPRDBF      POINT TO PRODUCT BUFFER                      
         BZ    XIT                 SKIP IF THERE IS NO BUFFER                   
*                                                                               
         XC    PGPAAABF,PGPAAABF   INIT PRODUCT AAA BILL FORMULA                
*                                                                               
PGAAALP  DS    0H                                                               
*                                                                               
         CLI   0(R5),0             DONE IF END OF TABLE REACHED                 
         BE    PGAAADN                                                          
*                                                                               
         USING PRDBUFFD,R5                                                      
*                                                                               
         CLC   PBFPRD,=C'AAA'       MATCH ON PRODUCT AAA                        
         BE    PGAAAFD                                                          
*                                                                               
PGAAACN  DS    0H                                                               
*                                                                               
         LA    R5,PBFLEN(R5)                                                    
         B     PGAAALP                                                          
*                                                                               
PGAAAFD  DS    0H                                                               
*                                                                               
         MVC   PGPAAABF,PBFBLBAS   SAVE PRODUCT AAA BILL FORMULA                
*                                                                               
PGAAADN  DS    0H                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
*         GET PRODUCT INFORMATION                                               
*                                                                               
PG1      ICM   R5,15,PBAPRDBF      POINT TO PRODUCT BUFFER                      
         BZ    XIT                                                              
*                                                                               
         USING PRDBUFFD,R5                                                      
*                                                                               
         MVC   PBPROD,PBFPRD       FIRST PRODUCT IN BUFFER                      
         MVC   PBPRDNM,PBFNAME                                                  
         MVC   PGPRDBF,PBFBLBAS    SAVE PRODUCT BILL FORMULA                    
         ST    R5,PRDADDR                                                       
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R3,1                R3=ESTIMATE NUMBER                           
*                                                                               
PG2      DS    0H                  GET PRODUCT DETAILS                          
*                                                                               
* BEGIN READING ESTIMATES FOR THIS PRODUCT  // ON CHANGE OF KEY (PRD)           
* READ NEXT PRODUCT IN PRODUCT TABLE (PRDADDR)                                  
*                                                                               
*                                                                               
         LA    R1,PGDATA                                                        
*                                                                               
PG5      CLI   0(R1),0                 CLEAR ALL P&G DATA FIELDS                
         BE    PG6                                                              
         L     RE,8(R1)            DISPLACEMENT INTO DATA                       
         LA    RE,CHGPR(RE)                                                     
         ZIC   RF,12(R1)                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         XC    0(0,RE),0(RE)                                                    
         LA    R1,16(R1)                                                        
         B     PG5                                                              
*                                                                               
PG6      BAS   RE,GETEST           GET EST & PG ESTREC                          
         BNE   XIT                 END OF ESTIMATES                             
*                                                                               
         OC    ACEST,ACEST         IF ACCOUNTING ESTIMATE GIVEN                 
         BZ    PG8A                  -NONE                                      
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         BAS   RE,READBLLS         READ BILL RECORDS                            
*                                                                               
         B     PG10A                                                            
*                                                                               
PG8A     DS    0H                                                               
*                                                                               
         XC    KEY,KEY             READ ESTIMATE BUCKET RECORD                  
         BAS   RE,READBKS                                                       
*                                                                               
PG10A    DS    0H                                                               
*                                                                               
         MVI   GLOPTS+1,C'Y'       F6 OPTION                      L01           
* * *    CLI   MLTBR,C'Y'                                         L01           
* * *    BE    *+12                                               L01           
         CLI   NOBRD,C'Y'                                         L01           
         BE    *+8                                                L01           
         MVI   GLOPTS+1,C'N'                                      L01           
*                                                                               
         CLI   RPTSCRN,X'E8'       SET OPTION IF EDIT LIST                      
         BNE   *+8                                                              
         MVI   GLOPTS+1,C'L'                                                    
*                                                                               
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER         CALL DRIVER FOR INPUT                        
         L     R5,SVREG5                                                        
*                                                                               
PG20     LA    R3,1(R3)            NEXT ESTIMATE                                
         B     PG6                                                              
*                                                                               
PGX      B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD01                                                      
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         TITLE 'PRWRI08 - READ ESTIMATE BUCKET RECORDS - READBKS'               
***********************************************************************         
*                                                                     *         
*        SUBROUTINE READS BUCKETS RECORDS                             *         
*         ALSO BUILDS TABLE OF YYMM$$$$                               *         
*         IN ORDERDOL                                                 *         
*                                                                     *         
*NTRY    R3 = ESTIMATE NUMBER                                         *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
READBKS  NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PBKRECD,R4          ESTABLISH ESTIMATE BUCKET REC KEY            
*                                                                               
         MVC   PBKKAGY,PBAGY                                                    
         MVC   PBKKMED,PBMED                                                    
         MVI   PBKKRCD,X'09'                                                    
         MVC   PBKKCLT,PBCLT                                                    
         MVC   PBKKPRD,PBPROD                                                   
         STH   R3,PBKKEST                                                       
*                                                                               
         XC    ORDERDOL,ORDERDOL   INIT DOLLAR AREA                             
*                                                                               
         GOTO1 HIGH                READ FOR BUCKET RECORD                       
*                                                                               
         CLC   KEY(PBKKEST-PBKRECD+L'PBKKEST),KEYSAVE  SAME A/M/C/P/EST         
         BNE   READBKSX            MUST FIND IT                                 
*                                                                               
         MVC   AIO,AIO1            READ INTO I/OAREA1                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO              ESTABLISH BUCKET RECORD                      
*                                                                               
*        BUILD DOLLAR TABLE                                                     
*                                                                               
*        FIND REGULAR BUCKET ELEMENT                                            
*                                                                               
         LA    R6,PBKRECD+33       POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
RBKREGLP DS    0H                                                               
*                                                                               
         USING BKELEMD,R6          ESTABLISH BUCKET ELEMENT                     
*                                                                               
         CLI   BKELEM,X'00'        DONE AT END OF RECORD                        
         BE    RBKREGDN                                                         
*                                                                               
         CLI   BKELEM,BKELREGQ     SKIP IF NOT REGULAR BUCKETS                  
         BNE   RBKREGCN                                                         
*                                                                               
         LA    R1,6                MAX SIX MONTHS                               
         LA    R2,REQMNTHS         POINT TO MONTH TABLE                         
         LA    R5,ORDERDOL         POINT TO DOLLAR STORAGE AREA                 
*                                                                               
RBKMNLP  DS    0H                                                               
*                                                                               
         CLC   0(2,R2),BKYM        MATCH ON YYMM                                
         BE    RBKMNFD                                                          
*                                                                               
RBKMNCN  DS    0H                                                               
*                                                                               
         LA    R5,8(R5)            POINT TO NEXT ORDERDOL                       
         LA    R2,2(R2)            POINT TO NEXT MONTH OF REQ                   
*                                                                               
         BCT   R1,RBKMNLP                                                       
*                                                                               
RBKMNDN  DS    0H                                                               
*                                                                               
         B     RBKREGCN            NO MATCH                                     
*                                                                               
RBKMNFD  DS    0H                                                               
*                                                                               
*        CALCULATE CLIENT COST -                                                
*           APPLY CURRENT BILL FORMULA TO ORDERED DOLLARS VIA GETCOST           
*                                                                               
         MVC   0(2,R5),BKYM        SAVE DATE                                    
*                                                                               
         TM    PBQPGOPT,PBQPGNTQ   IF NET DOLLARS WANTED                        
         BNO   *+14                                                             
         ZAP   2(6,R5),BKONET         RETURN NET DOLLARS                        
         B     RBKREGCN                                                         
*                                                                               
         ZAP   DUB,BKOGRS          CAPTURE GROSS                                
         CVB   RE,DUB              CVB                                          
         ST    RE,GCSTGRS          SET IN GETCOST WORKAREA                      
*                                                                               
         SP    DUB,BKONET          CALCULATE AGENCY COMM                        
         CVB   RE,DUB              CVB                                          
         ST    RE,GCSTAC           SET IN GETCOST WORKAREA                      
*                                                                               
         ZAP   DUB,BKOCD           CAPTURE CD                                   
         CVB   RE,DUB              CVB                                          
         ST    RE,GCSTCD           SET IN GETCOST WORKAREA                      
*                                                                               
         GOTO1 =V(GETCOST),DMCB,PGBLLFRM,GCSTWORK,0                             
*                                                                               
         L     RE,DMCB+4           GET RETURNED AMOUNT                          
         CVD   RE,DUB              CVD                                          
*                                                                               
         TM    PBQPGOPT,PBQPGACQ   IF AGENCY COMMISSION WANTED                  
         BNO   *+10                                                             
         SP    DUB,BKONET             SUBTRACT NET AMOUNT                       
*                                                                               
         ZAP   2(6,R5),DUB         SAVE AGENCY COST                             
*                                                                               
RBKREGCN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,BKELEM+1         BUMP TO NEXT ELEMENT                         
         LA    R6,BKELEM(RF)                                                    
*                                                                               
         B     RBKREGLP                                                         
*                                                                               
RBKREGDN DS    0H                                                               
*                                                                               
READBKSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=====================================================================*         
*                                                                     *         
*        IF WE ARE DEALING WITH A SHORT-RATE PG ESTIMATE RECORD       *         
*        SUBROUTINE READS BILL RECORDS FOR ORIGINAL ESTIMATE          *         
*        ALSO BUILDS TABLE OF YYMM$$$$                                *         
*        IN ORDERDOL                                                  *         
*                                                                     *         
*=====================================================================*         
         SPACE 2                                                                
READBLLS NTR1                                                                   
*                                                                               
*        INIT ACCUMULATORS                                                      
*                                                                               
         XC    ORDERDOL,ORDERDOL   REALLY BILLED DOLLARS HERE                   
*                                                                               
         LA    RF,ORDERDOL         INIT ACCUMULATORS                            
         LA    R0,6                6 BUCKETS                                    
*                                                                               
         ZAP   2(6,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PBILLRCD,R2         ESTABLISH KEY FOR BILLING RECORD             
*                                                                               
         MVC   PBILKAGY,PBAGY      AGENCY                                       
         MVC   PBILKMED,PBMED      MEDIA                                        
         MVI   PBILKRCD,X'08'      RECORD ID                                    
         MVC   PBILKCLT,PBCLT      CLIENT                                       
         MVC   PBILKPRD,PBPROD     PRODUCT                                      
         MVC   PBILKEST,ACEST      ACCOUNTING EST# FROM PGEST RECORD            
*                                                                               
         GOTO1 HIGH                FIND FIRST BILL                              
*                                                                               
RBLLOOP  DS    0H                                                               
*                                                                               
         CLC   KEY(PBILKMOS-PBILLRCD),KEYSAVE       SAME A/M/C/P/EST            
         BNE   RBLLPDN                                                          
*                                                                               
         GOTO1 GETREC              READ BILL RECORD                             
*                                                                               
         L     R2,AIO              POINT TO FOUND BILL RECORD                   
*                                                                               
         CLC   PBILLDAT,FCLYR      IGNORE IF BILL DATE BEFORE PGEST DTE         
         BL    RBLLPCN                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(3,BDTEYMD) BILL DATE TO YMD            
*                                                                               
*        ASSIGN BILLED AMOUNT TO CORRECT BUCKET                                 
*                                                                               
         LA    RF,ORDERDOL         POINT TO ACCUMULATORS                        
         LA    R0,6                NUMBER OF BUCKETS                            
         LA    R1,REQMNTHS         LIST OF MONTHS IN REQUEST PERIOD             
*                                                                               
RBLACCLP DS    0H                                                               
*                                                                               
         CLC   BDTEYM,0(R1)        MATCH ON YM                                  
         BE    RBLACCFD                                                         
*                                                                               
RBLACCCN DS    0H                                                               
*                                                                               
         LA    RF,8(RF)            POINT TO NEXT ORDERDOL                       
         LA    R1,2(R1)            POINT TO NEXT MONTH OF REQ                   
         BCT   R0,RBLACCLP                                                      
*                                                                               
         B     RBLACCDN            NO ACCUMULATOR FOUND                         
*                                                                               
RBLACCFD DS    0H                                                               
*                                                                               
         MVC   0(2,RF),BDTEYM      SAVE YEAR & MONTH OF BILL DATE               
         AP    2(6,RF),PBILLBIL    ADD BILLED AMOUNT TO ACCUMULATOR             
*                                                                               
RBLACCDN DS    0H                                                               
*                                                                               
RBLLPCN  DS    0H                                                               
*                                                                               
         LA    R2,KEY              RESET KEY POINTER                            
*                                                                               
         GOTO1 SEQ                 READ NEXT BILL RECORD                        
*                                                                               
         B     RBLLOOP                                                          
*                                                                               
RBLLPDN  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
* READ ESTIMATE HEADER *                                                        
GETEST   NTR1                                                                   
         SPACE 1                                                                
FE6      MVC   WORK(64),KEY        SAVE KEY/KEYSAVEE                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),PBQAGY                                                    
         MVC   KEY+2(1),PBQMED                                                  
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),PBQCLT                                                  
         MVC   KEY+7(3),PBPROD     MOVE PRD                                     
         STCM  R3,3,KEY+10           EST CODE                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(17),KEYSAVE                                                  
         BE    FE6FND              FOUND EST                                    
*                                                                               
         CLC   KEY(10),KEYSAVE       KEY SAME EXCEPT FOR EST                    
         BE    FE6ESTOK                                                         
*                                                                               
         CLC   KEY(7),KEYSAVE        KEY SAME EXCEPT FOR PRD/EST                
         BNE   NEXIT               CLIENT CHANGE                                
*                                                                               
         L     R5,PRDADDR                                                       
         LA    R5,PBFLEN(R5)                                                    
         ST    R5,PRDADDR                                                       
*                                                                               
         CLI   0(R5),0              END OF PRODUCTS                             
         BE    NEXIT                                                            
*                                                                               
         USING PRDBUFFD,R5                                                      
*                                                                               
         MVC   PBPROD,PBFPRD                                                    
         MVC   PBPRDNM,PBFNAME                                                  
         MVC   PGPRDBF,PBFBLBAS    PRODUCT BILL FORMULA                         
*                                                                               
         DROP  R5                                                               
*                                                                               
         LA    R3,1                R3=ESTIMATE NUMBER                           
*                                                                               
         B     FE6                                                              
*                                                                               
FE6ESTOK ICM   R3,3,KEY+10         INSERT NEW EST NUMBER IN REG                 
*                                                                               
FE6FND   L     R2,AIO1                                                          
         ST    R2,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING PESTRECD,R2                                                      
*                                                                               
*     SEE IF TEST ESTIMATE                                                      
*                                                                               
         MVC   PBEST,PESTKEST      FOR DRIVER                                   
         TM    PESTTEST,X'80'                                                   
         BNO   GETESTA                                                          
*                                                                               
GETNXTE  LA    R3,1(R3)                                                         
         B     FE6                                                              
*                                                                               
GETESTA  DS    0H                                                               
         OC    PBQESTB,PBQESTB                                                  
         BZ    ESTALL             ALL EST                                       
         CLC   PESTKEST,PBQESTB                                                 
         BL    GETNXTE                                                          
         BE    ESTALL                                                           
         CLC   PESTKEST,PBQESTBX   GROUP                                        
         BH    GETNXTE                                                          
**                                                                              
ESTALL   CLC   PESTST,PBQSTART                                                  
         BL    ESTLOW                                                           
         CLC   PESTST,PBQEND                                                    
         BH    GETNXTE                                                          
         B     GETDTOK                                                          
ESTLOW   CLC   PESTEND,PBQSTART                                                 
         BL    GETNXTE                                                          
GETDTOK  DS    0H                                                               
         MVC   ESTST,PESTST        SAVE EST START DATE                          
*                                                                               
*        FIND APPLICABLE BILLING FORMULA                                        
*                                                                               
         XC    PGBLLFRM,PGBLLFRM   INIT PROFILE SAVEAREA                        
*                                                                               
*        LOOK FOR BILLING OPTIONS IN THIS SEQUENCE                              
*         1-FOR THIS EST                                                        
*         2-FOR THIS EST PRODUCT AAA                                            
*         3-FOR THIS PRODUCT                                                    
*         4-FOR PRODUCT AAA                                                     
*         5-DEFAULT                                                             
*                                                                               
*        LOOK FOR THIS EST IN ESTIMAT TABLE // EST / PRD AAA IN PBESTAD         
*        PRODUCTS ARE IN PRODUCT TABLE                                          
*                                                                               
*        BILLING PROFILE IN ESTIMATE                                            
*                                                                               
         OC    PGBLLFRM(5),PESTBILP   USE THIS ESTIMATES PROFILE IF             
         BNZ   GESBPRFX                PRESENT                                  
*                                                                               
*        BILLING PROFILE IN ESTIMATE FOR PRODUCT AAA                            
*                                                                               
         XC    PGEAAABF,PGEAAABF   INIT ESTIMATE AAA BILL FORMULA               
*                                                                               
*        READ IN PRD AAA ESTIMATE FOR SAME ESTIMATE NUMBER                      
*                                                                               
         MVC   SAVPESTK,PESTREC    SAVE ESTIMATE RECORD KEY                     
*                                                                               
         LA    R2,KEY              ESTABLISH ESTIMATE KEY                       
         MVC   PESTKPRD,=C'AAA'    SWITCH TO PRODUCT AAA                        
*                                                                               
         GOTO1 HIGH                READ FOR PRD AAA ESTIMATE                    
*                                                                               
         CLC   PESTKEY,KEYSAVE     SKIP IF NOT FOUND                            
         BNE   GESEAAA9                                                         
*                                                                               
         MVC   AIO,AIO3            USE I/O AREA 3                               
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         MVC   AIO,AIO1            RESTORE I/O POINTER                          
*                                                                               
         L     R2,AIO3             POINT TO FOUND ESTIMATE                      
*                                                                               
         MVC   PGEAAABF,PESTBILP   SAVE ESTIMATE BILL FORMULA                   
*                                                                               
GESEAAA9 DS    0H                                                               
*                                                                               
         MVC   KEY,SAVPESTK        RESTORE ESTIMATE KEY                         
*                                                                               
         GOTO1 HIGH                RESTORE FILE POINTER                         
*                                                                               
         L     R2,AIO1             RESTORE ESTREC POINTER                       
*                                                                               
GESEAAAX DS    0H                                                               
*                                                                               
         OC    PGBLLFRM,PGEAAABF   BILL FORMULA FOR EST/PRD AAA                 
         BNZ   GESBPRFX            USE IF THERE IS ONE                          
*                                                                               
*        BILLING PROFILE FOR PRODUCT                                            
*                                                                               
         OC    PGBLLFRM,PGPRDBF    BILL FORMULA FOR PRODUCT                     
         BNZ   GESBPRFX            USE IF THERE IS ONE                          
*                                                                               
*        BILLING PROFILE FOR PRODUCT AAA                                        
*                                                                               
         OC    PGBLLFRM,PGPAAABF   BILL FORMULA FOR PRODUCT AAA                 
         BNZ   GESBPRFX            USE IF THERE IS ONE                          
*                                                                               
*        BILLING PROFILE FOR G-CD THE DEFAULT                                   
*                                                                               
         MVC   PGBLLFRM,=X'0505000000' USE DEFAULT G-CD                         
*                                                                               
GESBPRFX DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * *                                           
*---->  THIS IS AN ALL EST REQ      *                                           
* * * * * * * * * * * * * * * * * * *                                           
         SPACE 2                                                                
*                                                                               
*  READ PG ESTIMATE RECORD                                                      
*                                                                               
         MVC   SAVPESTK,PESTREC    SAVE ESTIMATE RECORD KEY                     
         XC    CHGPR(POLDATAL),CHGPR INIT PGEST DATA                            
*                                                                               
*        FIRST SEARCH IF POL PGESTREC RECORD FOR ESTIMATE FOUND YET             
*                                                                               
         XC    POLENTA,POLENTA     INIT TABLE ENTRY ADDRESS                     
*                                                                               
         LA    R5,POLBUFF          POINT TO BUFFER OF POL DATA                  
         OC    POLENDA,POLENDA     SKIP IF TABLE EMPTY                          
         BNL   PGE10                                                            
*                                                                               
         MVC   AIO,AIO2            USE I/O AREA 2                               
*                                                                               
POLLOOP DS     0H                                                               
*                                                                               
         USING POLDATAD,R5         ESTABLISH POOL ENTRY                         
*                                                                               
         CLC   POLEST,PBEST        MATCH ESIMATE NUMBER                         
         BE    PGE20                                                            
         LA    R5,POLENTL(R5)      BUMP TO NEXT ENTRY                           
         C     R5,POLENDA          TEST FOR END OF TABLE                        
         BL    POLLOOP                                                          
*                                                                               
*        RECORD NOT FOUND YET - GO READ IT                                      
*                                                                               
PGE10    DS    0H                                                               
*                                                                               
         MVC   KEY,SAVPESTK        SET UP KEY                                   
         MVI   KEY+3,X'0A'         RECORD ID                                    
         MVC   PESTKPRD-PESTKEY+KEY,=C'ZZZ'    SET FOR PRODUCT POL              
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
* IF POL PG EST RECORD IS NOT PRESENT .. LOOK FOR BRAND PG ESTIMATE             
*                                                                               
         CLC   KEY(17),KEYSAVE                                                  
         BNE   PGE25               NO POL PG ESTIMATE                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 PGFILL,PGSPARMS,POLDATA-POLDATAD(R5) FILL IN PG DATA             
*                                                                               
         LA    RF,POLENTL(R5)      RESET END OF DATA ADDRESS                    
         ST    RF,POLENDA                                                       
*                                                                               
PGE20    DS    0H                                                               
*                                                                               
         ST    R5,POLENTA          REMEMBER BUFFER ENTRY                        
*                                                                               
PGE25    DS    0H                                                               
*                                                                               
*        READ FOR BRAND'S PGESTREC FOR EST=000                                  
*                                                                               
         CLC   BRDPRD,PESTKPRD     SKIP IF PRODUCT'S EST 000 FOUND              
         BE    PGE40                                                            
*                                                                               
         XC    BRDDATA(POLDATAL),BRDDATA  INIT BRAND DATA                       
*                                                                               
         MVC   KEY,SAVPESTK            SET UP KEY                               
         MVI   KEY+3,X'0A'                                                      
         MVC   PESTKEST-PESTKEY+KEY,=X'0000'   SET FOR ESTIMATE 000             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
* SKIP IF BRAND PGEST 000 RECORD IS NOT PRESENT                                 
*                                                                               
         CLC   KEY(17),KEYSAVE                                                  
         BNE   PGE40               NO BRD PGEST FOR EST 000                     
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   BRDPRD,PGSTKPRD-PGSTKEY+KEY   SAVE PRODUCT CODE                  
*                                                                               
         GOTO1 PGFILL,PGSPARMS,BRDDATA FILL IN PG BRAND DATA                    
*                                                                               
*        SEARCH FOR BRAND PGESTREC FOR ESTIMATE                                 
*                                                                               
PGE40    DS    0H                                                               
*                                                                               
*        SEARCH FOR PGESTREC FOR BRAND AND ESTIMATE FROM PESTREC                
*                                                                               
         XC    BREDATA,BREDATA         INIT BRAND EST DATA                      
*                                                                               
         MVC   KEY,SAVPESTK            SET UP KEY                               
         MVI   KEY+3,X'0A'                                                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
* SAVE DATA FROM BRAND PGESTREC FOUND FOR ESTIMATE                              
*                                                                               
         CLC   KEY(17),KEYSAVE                                                  
         BNE   PGE50               NO BRD PGEST FOR EST 000                     
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 PGFILL,PGSPARMS,BREDATA FILL IN PG BRAND DATA                    
*                                                                               
PGE50    DS    0H                                                               
*                                                                               
*      IF THERE IS NO ESTIMATE 000 OR ACTUAL ESTIMATE DATA FOR A BRAND          
*          GO ON TO NEXT ESTIMATE ON FILE                                       
*                                                                               
         OC    BRDDATA,BRDDATA     USE ESTIMATE IF EST 00 DATA                  
         BNZ   *+10                                                             
         OC    BREDATA,BREDATA     OR ACTUAL ESTIMATE BRAND DATA EXISTS         
         BZ    GETNXTE                                                          
*                                                                               
*        FILL IN WORK FIELDS WITH CORRECT DATA                                  
*        USE BRAND ESTIMATE DATA IF FOUND                                       
*        ELSE USE POL DATA FOR ALL BUT BRAND CODE                               
*             USE BRAND ESTIMATE 000 FOR BRAND CODE                             
*                                                                               
         OC    BREDATA,BREDATA     SKIP IF NO BRAND ESTIMATE DATA               
         BZ    PGE55                                                            
         MVC   CHGPR(POLDATAL),BREDATA  USE BRAND ESTIMATE DATA                 
         B     PGE60                                                            
*                                                                               
PGE55    DS    0H                                                               
*                                                                               
         USING POLDATAD,R5         ESTABLISH TABLE ENTRY                        
         ICM   R5,15,POLENTA       POINT TO POL TABLE ENTRY                     
         BZ    *+10                NONE FOUND                                   
         MVC   CHGPR(POLDATAL),POLDATA                                          
*                                                                               
         MVC   BRAND,BRDBRAND      USE BRAND ESTIMATE 000 BRAND CODE            
*                                                                               
PGE60    DS    0H                                                               
*                                                                               
         LA    R1,PGDATA           SET ERROR CODES                              
*                                                                               
PGE70    CLI   0(R1),0                                                          
         BE    PGE80                                                            
         L     RE,8(R1)                                                         
         LA    RE,CHGPR(RE)        DATA FIELD                                   
         ZIC   RF,12(R1)                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SPACES                                                   
         BH    *+10                                                             
         OC    ERRCD,13(R1)                                                     
         LA    R1,16(R1)                                                        
         B     PGE70                                                            
*                                                                               
PGE80    DS    0H                                                               
*                                                                               
         CR    RE,RE                                                            
         XIT1  1,REGS=(R3)                                                      
*                                                                               
         DROP  R5,R2                                                            
         EJECT                                                                  
*                                                                               
*   PROCESS PG ESTIMATE RECORD                                                  
*        LOADS PGESTREC DATA INTO DATA FIELDS                                   
*        0(R1)==> FIELDS WHERE DATA IS TO BE STORED                             
*        IO1   CONTAINS PGESTREC                                                
*                                                                               
PGFILL   NTR1                                                                   
*                                                                               
         L     R2,0(R1)            POINT TO DATA AREA TO USE                    
         USING POLDATA,R2          ESTABLISH FIELDS                             
*                                                                               
         L     R5,AIO              POINT TO PGESTREC                            
         USING PGSTREC,R5         ESTABLISH PGESTREC                            
*                                                                               
         LA    R5,PGSTKEDQ(R5)       SCAN ELEMENTS FOR PG DATA                  
         SR    R0,R0                                                            
*                                                                               
PG7      CLI   0(R5),0             EOR                                          
         BE    PG12                                                             
         CLI   0(R5),PGSTEIDQ      REQUIRED DATA ELEMENT                        
         BNE   PG10                                                             
         LA    R1,PGDATA                                                        
*                                                                               
PG8      CLI   0(R1),0                                                          
         BE    PG10                                                             
         CLC   PGSTNAME-PGSTELEM(L'PGSTNAME,R5),0(R1)                           
         BE    *+12                                                             
         LA    R1,16(R1)                                                        
         B     PG8                                                              
         ZIC   RE,12(R1)                                                        
         BCTR  RE,0                                                             
         L     RF,8(R1)            SAVEAREA DISPLACEMENT                        
         LA    RF,0(RF,R2)                                                      
         EX    RE,*+4                                                           
         MVC   0(0,RF),PGSTDATA-PGSTELEM(R5)                                    
*                                                                               
         CLC   PGSTNAME-PGSTELEM(L'PGSTNAME,R5),=CL8'ACCOUNT'  IF ACCT          
         BNE   PG801                                                            
*                                                                               
         OC    3(3,RF),3(RF)       IF OLD FORMAT                                
         BNZ   *+16                                                             
         MVC   3(3,RF),PGSTDATA-PGSTELEM(R5)   RIGHT JUSTIFY                    
         MVC   0(3,RF),=CL3' '                                                  
*                                                                               
PG801    DS    0H                                                               
*                                                                               
         CLC   PGSTNAME-PGSTELEM(L'PGSTNAME,R5),=CL8'BRAND  '  IF BRAND         
         BNE   PG802                                                            
*                                                                               
         CLI   3(RF),0             IF OLD FORMAT                                
         BNZ   *+14                                                             
         MVC   1(3,RF),PGSTDATA-PGSTELEM(R5)   RIGHT JUSTIFY                    
         MVI   0(RF),C'0'                                                       
*                                                                               
PG802    DS    0H                                                               
*                                                                               
PG10     IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     PG7                                                              
*                                                                               
PG12     MVI   ERRCD,0             INITIALIZE ERROR CODE                        
         LA    R1,BRANDTAB                                                      
         MVC   BRDCD,=C'0000'      SET BRAND CODE (FROM TABLE)                  
*                                                                               
PG13     CLI   0(R1),0                                                          
         BNE   *+12                                                             
         OI    ERRCD,ERRBRC                                                     
         B     PG14                                                             
         CLC   PBAGY,0(R1)                                                      
         BNE   PG13C                                                            
         MVC   BRDCD,2(R1)                                                      
*                                                                               
         CLC   PBAGY,=C'H9'        IF AGENCY SMG                                
         BNE   PG13AB                                                           
*                                                                               
         CLC   PBQCLT,=C'PGB'         IF CLIENT PGB                             
         BE    *+10                                                             
         CLC   PBQCLT,=C'PG1'         OR CLIENT PG1                             
         BNE   *+10                                                             
         MVC   BRDCD,=C'0301'            ASSIGN TO AGENCY 1                     
*                                                                               
PG13AB   DS    0H                                                               
         B     PG14                                                             
*                                                                               
PG13C    DS    0H                                                               
         LA    R1,5(R1)                                                         
         B     PG13                                                             
*                                                                               
BRANDTAB DC    CL2'H9',CL4'0302'    BRAND CODE TABLE                            
         DC    CL2'NW',CL4'0304'    BRAND CODE TABLE                            
         DC    CL2'DF',CL4'0303'                                                
         DC    X'00'                                                            
*                                                                               
PG14     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DS    0F                                                               
PGDATA   DC    CL8'CHRG PER',AL4(CHGPR-CHGPR)                                   
         DC    AL1(L'CHGPR,ERRCHP,0,0)                                          
         DC    CL8'ACCOUNT ',AL4(ACCT-CHGPR)                                    
         DC    AL1(L'ACCT,ERRACC,0,0)                                           
         DC    CL8'BRAND   ',AL4(BRAND-CHGPR)                                   
         DC    AL1(L'BRAND,ERRBRN,0,0)                                          
         DC    CL8'ESTIMATE',AL4(ESTIM-CHGPR)                                   
         DC    AL1(L'ESTIM,ERREST,0,0)                                          
         DC    CL8'EVENT CD',AL4(EVNCD-CHGPR)                                   
         DC    AL1(L'EVNCD,ERREVC,0,0)                                          
         DC    CL8'MLT-BRND',AL4(MLTBR-CHGPR)                                   
         DC    AL1(L'MLTBR,ERRMBR,0,0)                                          
         DC    CL8'NOBRAND ',AL4(NOBRD-CHGPR)                                   
         DC    AL1(L'NOBRD,0,0,0)                                               
         DC    CL8'FISYREND',AL4(FCLYR-CHGPR)                                   
         DC    AL1(L'FCLYR,0,0,0)                                               
         DC    CL8'ACCEST  ',AL4(ACEST-CHGPR)                                   
         DC    AL1(L'ACEST,0,0,0)                                               
         DC    CL8'BRND SUF',AL4(BRSFX-CHGPR)                                   
         DC    AL1(L'BRSFX,0,0,0)                                               
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         DS    0F                                                               
SVREG5   DS    F                                                                
PRDADDR  DS    F                                                                
PGSPARMS DS    6A                  PROGRAM PARAMETER LIST                       
SAVPESTK DS    XL(L'PESTKEY)       ESTIMATE KEY SAVEAREA                        
FCLYRCH  DS    CL6                 ACCOUNTING DATE YYMMDD                       
BDTEYM   DS    0XL2                YM OF BILL DATE                              
BDTEYMD  DS    XL3                 BILL DATE YMD                                
*                                                                               
ESTST    DS    CL6                 ESTIMATE START DATE                          
BRDCD    DS    CL4                 BRAND CODE                                   
*                                                                               
CHGPR    DS    CL3                 PG DATA FIELDS                               
ACCT     DS    CL6                                                              
BRAND    DS    CL4                                                              
ESTIM    DS    CL4                                                              
EVNCD    DS    CL6                                                              
MLTBR    DS    CL1                                                              
NOBRD    DS    CL1                 L01                                          
FCLYR    DS    CL6                 FISCAL YEAR END - YYMMDD                     
ACEST    DS    XL2                 ACCOUNTING ESTIMATE                          
BRSFX    DS    XL2                 BRAND SUFFIX                                 
*                                                                               
CHGPRSV  DS    CL3                 PG DATA FIELDS                               
BRANDSV  DS    CL4                                                              
ESTIMSV  DS    CL4                                                              
BRDCDSV  DS    CL4                 BRAND CODE                                   
*                                                                               
ERRCD    DS    XL1                 ERROR BYTE                                   
ERRCHP   EQU   X'80'               CHARGE PERIOD                                
ERRACC   EQU   X'40'               ACCOUNT                                      
ERRBRN   EQU   X'20'               BRAND                                        
ERREST   EQU   X'10'               ESTIMATE                                     
ERREVC   EQU   X'08'               EVENT CODE                                   
ERRMBR   EQU   X'04'               MULTI BRAND                                  
ERRBRC   EQU   X'02'               'NO BRAND' CODE                              
ERRAGY   EQU   X'01'               AGENCY                         L01           
*                                                                               
F4ERRMSK DC    AL1(ERRCHP+ERRACC+ERRAGY+ERRBRN+ERREST+ERREVC+ERRMBR)            
F5ERRMSK DC    AL1(ERRCHP+ERRACC+ERRAGY+ERRBRN+ERREST)            L01           
F6ERRMSK DC    AL1(ERRCHP+ERRACC+ERRBRN+ERREST+ERRAGY+ERRBRC)     L01           
*                                                                               
MBROPT   DS    CL1                 MULTI-BRAND OPTION                           
REQMNTHS DS    XL14                REQUEST MONTHS, 2 X 6 MONTHS 1 EOT           
ESTSTMN  DS    XL2                 ESTIMATE START MONTH                         
ESTENMN  DS    XL2                 ESTIMATE END MONTH                           
ORDERDOL DS    CL96                ORDERED DOLLARS X 12 MONTHS YYMMPL6$         
*AMONTHS  DC    6XL2'00'                                                        
*                                                                               
PGEAAABF DS    CL5                 EST/PRD AAA BILLING FORMULA                  
PGPRDBF  DS    CL5                 PRODUCT     BILLING FORMULA                  
PGPAAABF DS    CL5                 PRODUCT AAA BILLING FORMULA                  
PGBLLFRM DS    CL5                 BILLING FORMULA                              
*                                                                               
*        GETCOST WORKAREA                                                       
*                                                                               
GCSTWORK DS    0F                                                               
GCSTGRS  DS    F                   GROSS                                        
GCSTCD   DS    F                   CD                                           
GCSTAC   DS    F                   AGENCY COMM                                  
*                                                                               
BRDPRD   DS    CL3                 BRAND SYSTEM PRODUCT CODE                    
*                                                                               
BRDDATA  DS    0XL(POLDATAL)       PG DATA FOR BRD EST 000                      
BRDCHGPR DS    CL3                 PG DATA FIELDS                               
BRDACCT  DS    CL6                 ACCOUNT NUMBER                               
BRDBRAND DS    CL4                 BRAND CODE                                   
BRDESTIM DS    CL4                 ESTIMATE NUMBER                              
BRDEVNCD DS    CL6                 EVENT CODE                                   
BRDMLTBR DS    CL1                 MULTIPLE BRAND INDICATOR                     
BRDNOBRD DS    CL1                 NO BRAND INDICATOR                           
BRDFCLYR DS    CL6                 FISCAL YEAR END - YYMMDD                     
BRDACEST DS    XL2                 ACCOUNTING ESTIMATE                          
BRDBRSFX DS    XL2                 BRAND SUFFIX                                 
BRDDATAL EQU   *-BRDDATA           DATA LENGTH                                  
*                                                                               
BREDATA  DS    0XL(POLDATAL)       PG DATA FOR BRD EST >000                     
BRECHGPR DS    CL3                 PG DATA FIELDS                               
BREACCT  DS    CL6                 ACCOUNT NUMBER                               
BREBRAND DS    CL4                 BRAND CODE                                   
BREESTIM DS    CL4                 ESTIMATE NUMBER                              
BREEVNCD DS    CL6                 EVENT CODE                                   
BREMLTBR DS    CL1                 MULTIPLE BRAND INDICATOR                     
BRENOBRD DS    CL1                 NO BRAND INDICATOR                           
BREFCLYR DS    CL6                 FISCAL YEAR END - YYMMDD                     
BREACEST DS    XL2                 ACCOUNTING ESTIMATE                          
BREBRSFX DS    XL2                 BRAND SUFFIX                                 
BREDATAL EQU   *-BREDATA           DATA LENGTH                                  
*                                                                               
POLENTA DC     A(0)                CURRENT ENTRY ADDRESS                        
POLENDA DC     A(POLBUFF)          END OF TABLE ADDRESS                         
*                                                                               
POLBUFF  DC    100XL(POLENTL)'00' POLDATA BUFFER                                
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR POOL (ZZZ) PG EST DATA                             *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
POLDATAD DSECT                                                                  
POLENT   DS    0X                  ENTRY IN TABLE                               
*                                  KEY FIELDS                                   
POLEST   DS    XL2                 ESTIMATE NUMBER                              
*                                                                               
POLDATA  DS    0X                                                               
POLCHGPR DS    CL3                 PG DATA FIELDS                               
POLACCT  DS    CL6                 ACCOUNT NUMBER                               
POLBRAND DS    CL4                 BRAND CODE                                   
POLESTIM DS    CL4                 ESTIMATE NUMBER                              
POLNCD DS    CL6                 EVENT CODE                                     
POLMLTBR DS    CL1                 MULTIPLE BRAND INDICATOR                     
POLNOBRD DS    CL1                 NO BRAND INDICATOR                           
POLFCLYR DS    CL6                 FISCAL YEAR END - YYMMDD                     
POLACEST DS    XL2                 ACCOUNTING ESTIMATE                          
POLBRSFX DS    XL2                 BRAND SUFFIX                                 
POLDATAL EQU   *-POLDATA           DATA LENGTH                                  
POLENTL  EQU   *-POLENT            TABLE ENTRY LENGTH                           
*                                                                               
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*PPWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*PRGENFILE                                                                      
*PRWRIFFD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE PRWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
       ++INCLUDE PRGENFILE                                                      
         PRINT   ON                                                             
       ++INCLUDE PRWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE PRWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRWRIE7D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091PRWRI08   07/17/02'                                      
         END                                                                    
