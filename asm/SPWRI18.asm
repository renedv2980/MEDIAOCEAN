*          DATA SET SPWRI18    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T20418A,*                                                                
         TITLE 'T20418 - PC DATA INTERFACE'                                     
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
************** MODULE NOT IN USE **************************************         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*--DATE---LVL-BY-----CHANGE-------------------------------------------*         
* 16JUN98 HISTORY LOST                                                *         
* 16JUN98 15  NRK -- Y2K COMPLIANCE                                   *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T20418   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20418,RA                                                  
         LR    R8,RC                                                            
         USING WORKD,R8                                                         
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      SPOTIO HOOK                                  
         BE    INPUT                                                            
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
INIT     OI    SBQSKIP,SBQSKGL     READ BUYS ONLY                               
         OI    SBQSKIP,SBQSKBIL                                                 
         MVI   SBQSEPES,C'Y'       ENSURE EST=ALL                               
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         MVI   SBEDEMTY,C'A'       GET AFFID DEMOS                              
         MVI   SBEUNALL,C'N'       NO UNALLOCATED                               
         MVI   NDEMOS,4            GET 4 DEMOS                                  
         OI    DATAIND,DILEN       GET SPOT LENGTH BREAKOUT                     
         OI    DATAIND2,DIEST      ESTIMATE                                     
         XC    LEVELS,LEVELS                                                    
         OI    DOWNOPT,GLDLACTV    TURN ON DOWNLOADING                          
         OI    DOWNOPT,GLDLNOTR    WITH NO TRUNCATION                           
         OI    DOWNOPT,GLDLNOHD    AND NO HEADINGS                              
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    CLI   SBQBPRD,X'FF'       TEST PRD=POL REQUEST                         
         BNE   *+12                NO                                           
         MVI   SBQBPRD,0           YES-BREAK OUT THE PRODUCTS                   
         OI    SBQPIND,SBQPOLSP                                                 
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
* SPOTIO INPUT HOOK                                                             
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCSP     BUY RECORDS                                  
         BE    BUY                                                              
         B     XIT                                                              
         EJECT                                                                  
* BUY RECORD HOOK                                                               
*                                                                               
BUY      L     R6,SBAIO1                                                        
         USING BUYRECD,R6                                                       
         MVC   SBEMKT,SBBMKT       EXTRACT MKT FROM DIRECTORY POINTER           
         CLI   SBQSPILL,C'N'       TEST SPILL REQUESTED                         
         BNE   *+14                                                             
         CLC   SBBMKT,BUYMSTA      NO-REJECT SPILL MKT                          
         BNE   BUYX                                                             
         MVC   SBBMKT,BUYMSTA                                                   
         CLI   SBQSVI,0            TEST SVI ADJUST SET YET                      
         BNE   BUY2                                                             
         ZIC   RE,BUYKPRD          NO-PICK UP FROM EST BUFFER                   
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         ZIC   RF,BUYKEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)          POINTER TO ESTIMATE BUFFER ENTRY             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(ESTBUFFL)                                                  
         A     R1,SBAESTBF                                                      
         USING ESTBUFFD,R1                                                      
         ZIC   RE,EBSVI                                                         
         SRL   RE,4                                                             
         STC   RE,SBESVI                                                        
         DROP  R1                                                               
*                                                                               
BUY2     MVI   SVPRD,0                                                          
         MVI   SVPRD2,0                                                         
         GOTO1 SPOTBUY,DMCB,SBLOCK                                              
         L     R5,SBASPTTB                                                      
         LA    R5,0(R5)                                                         
         ICM   R0,15,SBNSPTEN                                                   
         BZ    BUYX                                                             
         USING SPTTABD,R5                                                       
*                                                                               
BUY4     TM    SPTIND,SPTDUMMY     MAYBE A DUMMY ENTRY                          
         BO    BUY10                                                            
         CLC   SPTPRD1,SVPRD       TEST PRODUCT CHANGE                          
         BE    BUY6                                                             
         ZIC   RE,SPTPRD1          YES-                                         
         STC   RE,SVPRD                                                         
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         LA    R1,0(RE,R1)                                                      
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD,PBALPH        SET ALPHA CODE                               
         LA    RE,SBEDEMOS                                                      
         OC    SBEDEMOS,SBEDEMOS   TEST DEMO LIST OVERRIDE                      
         BNZ   BUY5                                                             
         ZIC   RE,SPTPRD1          FIND ENTRY IN PRD/EST TABLE                  
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         ZIC   RF,BUYKEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         A     RE,SBAESTTB                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(RE)          POINTER TO ESTIMATE BUFFER ENTRY             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         MH    R1,=Y(ESTBUFFL)                                                  
         A     R1,SBAESTBF                                                      
         USING ESTBUFFD,R1                                                      
         LA    RE,EBDEMOS           EXTRACT A(DEMOS)                            
         DROP  R1                                                               
*                                                                               
BUY5     ST    RE,ADEMLST                                                       
*                                                                               
BUY6     CLC   SPTPRD2,SVPRD2      TEST PRODUCT 2 CHANGE                        
         BE    BUY8                                                             
         ZIC   RE,SPTPRD2          YES-                                         
         STC   RE,SVPRD2                                                        
         BCTR  RE,0                                                             
         MH    RE,=Y(PRDBUFFL)                                                  
         L     R1,SBAPRDBF                                                      
         LA    R1,0(RE,R1)                                                      
         USING PRDBUFFD,R1                                                      
         MVC   SBPRD2,PBALPH                                                    
*                                                                               
BUY8     ST    R5,SBACURCH                                                      
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         BAS   RE,GODRIVER                                                      
*                                                                               
BUY10    A     R5,SBLSPTEN         NEXT SPOT TABLE ENTRY                        
         BCT   R0,BUY4                                                          
*                                                                               
BUYX     MVI   RPMODE,RPSKIOHK     SKIP SPWRI01'S PROCBUY                       
         B     XIT                                                              
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
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
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
         DC    CL8'OMED    ',A(OMED)                                            
         DC    CL8'IPRD    ',A(IPRD)                                            
         DC    CL8'OSTA    ',A(OSTA)                                            
         DC    CL8'IDPT    ',A(IDPT)                                            
         DC    CL8'ILEN    ',A(ILEN)                                            
         DC    CL8'OLEN    ',A(OLEN)                                            
         DC    CL8'ISTART  ',A(ISTART)                                          
         DC    CL8'OSTART  ',A(OSTART)                                          
         DC    CL8'ISTDAY  ',A(ISTDAY)                                          
         DC    CL8'IDAYS   ',A(IDAYS)                                           
         DC    CL8'OSTIME  ',A(OSTIME)                                          
         DC    CL8'OETIME  ',A(OETIME)                                          
         DC    CL8'ISPTS   ',A(ISPTS)                                           
         DC    CL8'ICOST   ',A(ICOST)                                           
         DC    CL8'IPRD2   ',A(IPRD2)                                           
         DC    CL8'ILEN2   ',A(ILEN2)                                           
         DC    CL8'IAFDAY  ',A(IAFDAY)                                          
         DC    CL8'IAFTIME ',A(IAFTIME)                                         
         DC    CL8'OAFTIME ',A(OAFTIME)                                         
         DC    CL8'OADJ    ',A(OADJ)                                            
         DC    CL8'IPURP   ',A(IPURP)                                           
         DC    CL8'ISPEC   ',A(ISPEC)                                           
         DC    CL8'ISEQ    ',A(ISEQ)                                            
         DC    CL8'IDEMCD  ',A(IDEMCD)                                          
         DC    CL8'IDEMOV  ',A(IDEMOV)                                          
         DC    CL8'IDEMVAL ',A(IDEMVAL)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     R5,SBACURCH         R5=A(SPOT TABLE ENTRY)                       
         USING SPTTABD,R5                                                       
         L     R6,SBAIO1           R6=A(BUY RECORD)                             
         USING BUYRECD,R6                                                       
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   *+8                                                              
         MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
OMED     MVC   0(1,R3),0(R2)       MEDIA                                        
         B     XIT                                                              
*                                                                               
IPRD     MVC   0(3,R2),SBPRD       PRODUCT                                      
         B     XIT                                                              
*                                                                               
IPRD2    MVC   0(3,R2),SBPRD2      PRODUCT 2                                    
         B     XIT                                                              
*                                                                               
OSTA     MVC   0(5,R3),0(R2)       STATION                                      
         CLI   4(R2),C' '                                                       
         BNE   XIT                                                              
         MVI   4(R3),C'T'                                                       
         B     XIT                                                              
*                                                                               
IDPT     MVC   0(1,R2),BDDAYPT     DAYPART                                      
         B     XIT                                                              
*                                                                               
ILEN     MVC   0(1,R2),SPTSLN1     SPOT LENGTH                                  
         B     XIT                                                              
*                                                                               
ILEN2    MVC   0(1,R2),SPTSLN2     SPOT LENGTH 2                                
         B     XIT                                                              
*                                                                               
OLEN     ZIC   RE,0(R2)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         B     XIT                                                              
*                                                                               
ISTART   MVC   0(3,R2),BDSTART     START DATE                                   
         B     XIT                                                              
*                                                                               
OSTART   EQU   *                                                                
*Y2K         GOTO1 DATCON,DMCB,(3,(R2)),DUB                                     
         GOTO1 DATCON,DMCB,(3,(R2)),(X'20',DUB) NO FUNNY YEARS                  
         MVC   0(2,R3),DUB                                                      
         MVI   2(R3),C'/'                                                       
         MVC   3(2,R3),DUB+2                                                    
         MVI   5(R3),C'/'                                                       
         MVC   6(2,R3),DUB+4                                                    
         B     XIT                                                              
*                                                                               
ISTDAY   GOTO1 DATCON,DMCB,(3,BDSTART),DUB    START DAY OF WEEK                 
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),BLANKS                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R2),0(R1)                                                    
         OI    0(R2),X'F0'                                                      
         B     XIT                                                              
*                                                                               
IDAYS    SR    RF,RF               ACTIVE DAYS                                  
         ICM   RF,8,BDDAY                                                       
         LA    R1,0(R2)                                                         
         LA    R0,7                                                             
IDAYS2   MVI   0(R1),C'0'                                                       
         SLL   RF,1                                                             
         LTR   RF,RF                                                            
         BNM   *+8                                                              
         MVI   0(R1),C'1'                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,IDAYS2                                                        
         B     XIT                                                              
*                                                                               
OSTIME   MVC   CARD,BLANKS         START TIME                                   
         XC    ENDTIME,ENDTIME                                                  
         GOTO1 UNTIME,DMCB,(R2),CARD                                            
         L     R5,SBAIO1                                                        
         GOTO1 SCANNER,DMCB,(C'C',CARD),(1,(R5)),C',=,-'                        
         CLI   4(R1),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R2,R3                                                            
         LA    R6,12(R5)                                                        
         ZIC   RE,0(R5)                                                         
*                                                                               
OSTIME2  MVC   0(4,R2),=C'0000'                                                 
         MVI   4(R2),C' '                                                       
         LA    RF,0(RE,R6)                                                      
         BCTR  RF,0                                                             
         CLI   0(RF),C'0'                                                       
         BNL   *+12                                                             
         MVC   4(1,R2),0(RF)                                                    
         BCTR  RE,0                                                             
         LNR   RF,RE                                                            
         LA    RF,4(RF,R2)                                                      
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),0(R6)                                                    
         OC    ENDTIME,ENDTIME                                                  
         BNZ   OSTIME4                                                          
         CLI   1(R5),0                                                          
         BNE   *+14                                                             
         MVC   ENDTIME,0(R3)                                                    
         B     OSTIME4                                                          
         LA    R2,ENDTIME                                                       
         LA    R6,22(R5)                                                        
         ZIC   RE,1(R5)                                                         
         B     OSTIME2                                                          
*                                                                               
OSTIME4  CLI   4(R3),C' '                                                       
         BH    XIT                                                              
         MVC   4(1,R3),ENDTIME+4                                                
         B     XIT                                                              
*                                                                               
OETIME   MVC   0(5,R3),ENDTIME     END TIME                                     
         B     XIT                                                              
*                                                                               
ISPTS    MVC   0(2,R2),SPTSPOTS    SPOTS                                        
         B     XIT                                                              
*                                                                               
ICOST    L     R1,SPTGRS1          GROSS COST                                   
         A     R1,SPTGRS2                                                       
         ST    R1,0(R2)                                                         
         B     XIT                                                              
*                                                                               
IAFDAY   MVI   0(R2),C'0'          AFFIDAVIT DAY                                
         OC    SPTADATE,SPTADATE                                                
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(2,SPTADATE),DUB                                     
         GOTO1 GETDAY,DMCB,DUB,FULL                                             
         CLC   FULL(3),BLANKS                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R2),0(R1)                                                    
         OI    0(R2),X'F0'                                                      
         B     XIT                                                              
*                                                                               
IAFTIME  MVC   0(2,R2),SPTATIME    AFFIDAVIT TIME                               
         B     XIT                                                              
*                                                                               
OAFTIME  MVC   0(5,R3),=C'0000 '                                                
         OC    0(2,R2),0(R2)                                                    
         BZ    XIT                                                              
         MVC   FULL(2),0(R2)                                                    
         MVC   FULL+2(2),FULL                                                   
         MVC   WORK(11),BLANKS                                                  
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         LA    R1,WORK+10                                                       
         LA    R0,10                                                            
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                                                             
         MVC   4(1,R3),0(R1)                                                    
         LA    RE,3(R3)                                                         
         SR    RF,RF                                                            
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         CLI   0(R1),C'-'                                                       
         BE    *+18                                                             
         BCTR  R1,0                                                             
         BCTR  RE,0                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-16                                                          
         DC    H'0'                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),1(R1)                                                    
         B     XIT                                                              
*                                                                               
OADJ     MVC   0(1,R3),0(R2)       ADJACENCY CODE                               
         MVI   1(R3),C' '                                                       
         B     XIT                                                              
*                                                                               
IPURP    MVC   0(1,R2),BDPURP      PURPOSE CODE                                 
         B     XIT                                                              
*                                                                               
ISPEC    MVI   0(R2),C'N'          SPECIAL PROGRAMMING                          
         CLI   BDPROGRM+17,0                                                    
         BNE   XIT                                                              
         MVI   0(R2),C'Y'                                                       
         B     XIT                                                              
*                                                                               
ISEQ     L     R1,SEQNUM           SEQUENCE NUMBER                              
         A     R1,=F'1'            (FOR UNIQUE RECORDS)                         
         ST    R1,SEQNUM                                                        
         ST    R1,0(R2)                                                         
         B     XIT                                                              
*                                                                               
IDEMCD   L     R1,ADEMLST          DEMO CODE                                    
         ZIC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         LA    R1,0(RE,R1)                                                      
         MVC   0(1,R2),1(R1)                                                    
         ZIC   RE,2(R1)                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R2),DUB                                                      
         B     XIT                                                              
*                                                                               
IDEMOV   MVI   0(R2),C'N'          DEMO OVERRIDE INDICATOR                      
         B     XIT                                                              
*                                                                               
IDEMVAL  ZIC   R1,GLARGS           DEMO VALUE                                   
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,SPTDEMOS(R1)                                                  
         MVC   0(4,R2),0(R1)                                                    
         B     XIT                                                              
         EJECT                                                                  
         GOTO1 DRIVER,DMCB,(R4)                                                 
***********************************************************************         
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                   *         
***********************************************************************         
         SPACE 1                                                                
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
SEQNUM   DC    F'0'                                                             
*                                                                               
ENDTIME  DS    CL5                                                              
SVPRD    DS    XL1                                                              
SVPRD2   DS    XL1                                                              
CARD     DS    CL80                                                             
*                                                                               
ZEROS    DC    CL8'00000000'                                                    
XFF      DC    XL12'FFFFFFFFFFFFFFFFFFFFFFFF'                                   
BLANKS   DC    CL80' '                                                          
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    F                                                                
         DS    F                                                                
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENBUY                                                                       
*SPWRIFFD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIEDD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPWRI18   05/01/02'                                      
         END                                                                    
