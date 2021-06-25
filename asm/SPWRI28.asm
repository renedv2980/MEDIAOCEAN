*          DATA SET SPWRI28    AT LEVEL 022 AS OF 12/16/04                      
*PHASE T20428A,*                                                                
         TITLE 'T20428 - S C JOHNSON TAPE INTERFACE'                            
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
*                                                                     *         
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
*--DATE---LVL-BY-----CHANGE-------------------------------------------*         
* 16JUN98 HISTORY LOST                                                *         
* 16JUN98 20  NRK -- Y2K COMPLIANCE                                   *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T20428   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20428,RA                                                  
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
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1)                                         
         ST    R1,ASAVE                                                         
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPFIRST      FIRST TIME FOR REQUEST                       
         BE    FRST                                                             
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPINPUT      SPOTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL CALL FOR REQUEST                       
         BE    FINAL                                                            
         CLI   RPMODE,RPRUNLST     RUN LAST                                     
         BE    RUNLST                                                           
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
* REQUEST FIRST                                                                 
*                                                                               
FRST     CLI   TWAFIRST,0          TEST FIRST REQUEST                           
         BNE   *+12                                                             
         MVI   DCBOPEN,C'N'        YES-DCB NOT OPEN YET                         
         B     XIT                                                              
         L     RE,ASAVE            NO-RESTORE SAVED VALUES                      
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     MVI   REPTYPE,C'M'        REPORT IS SPOT MEDIA                         
         CLC   CONREC(4),=C'ISCJ'                                               
         BNE   *+8                                                              
         MVI   REPTYPE,C'I'        OR SPOT INVOICE                              
*                                                                               
         MVI   SUPTAPE,C'N'        OPTION TO SUPPRESS THE TAPE                  
         LA    R2,SCJTAPH                                                       
         CLI   5(R2),0                                                          
         BE    INIT1                                                            
         MVC   SUPTAPE,8(R2)                                                    
         CLI   SUPTAPE,C'Y'                                                     
         BE    INIT1                                                            
         CLI   SUPTAPE,C'N'                                                     
         BNE   EINV                                                             
*                                                                               
INIT1    LA    R2,SCJTITH          VALIDATE THE TITLE                           
         MVC   TITLE,BLANKS                                                     
         MVC   TITLE(27),=C'S C JOHNSON SPOT MEDIA TAPE'                        
         CLC   CONREC(4),=C'ISCJ'                                               
         BNE   *+10                                                             
         MVC   TITLE(29),=C'S C JOHNSON SPOT INVOICE TAPE'                      
         CLI   5(R2),0                                                          
         BE    INIT2                                                            
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT2    GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         OI    SBQSKIP,SBQSKGL     TELL SPOTIO NOT TO READ GOALS                
         CLI   REPTYPE,C'I'                                                     
         BNE   *+12                                                             
         OI    SBQSKIP,SBQSKBUY    NO BUYS FOR INVOICE TAPE                     
         B     INIT3                                                            
         OI    SBQSKIP,SBQSKBIL    NO BILLS FOR MEDIA TAPE                      
         OI    DATAIND,DILEN       SPOT LENGTH BREAKOUT                         
         OI    DATAIND,DIDEMA+DIDEMP  AFFID AND PURCH DEMOS                     
         MVI   NDEMOS,1            1 DEMO                                       
         XC    SVTGTDEM,SVTGTDEM                                                
*                                                                               
INIT3    CLI   SBQBPRD,X'FF'       TRANSLATE PRD=POL TO PRD=ALL                 
         BNE   *+8                                                              
         MVI   SBQBPRD,0                                                        
         MVI   SBQSEPES,C'Y'       ENSURE EST=ALL                               
         MVI   SBQPER,SBQPWK       WEEKLY BREAKOUT                              
         MVI   SBQPERLO,1          ALL WEEKS                                    
         MVI   SBQPERHI,X'FF'                                                   
         MVI   MYFIRSTH,10         FIRST HEADING ON HEAD10                      
*                                                                               
         XC    LEVELS,LEVELS                                                    
         LA    R1,LEVELS           SET THE LEVELS                               
         LA    R5,RPTLEVS1                                                      
         CLI   REPTYPE,C'I'                                                     
         BNE   *+8                                                              
         LA    R5,RPTLEVS2                                                      
         LA    RF,1                                                             
*                                                                               
INIT4    CLI   0(R5),X'FF'                                                      
         BE    INITX                                                            
         MVC   0(1,R1),0(R5)                                                    
         CLI   0(R5),QMED                                                       
         BNE   *+8                                                              
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         LA    R5,1(R5)                                                         
         B     INIT4                                                            
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS1 DC    AL1(QMED)                                                        
         DC    AL1(QBUY)                                                        
         DC    AL1(QTARGET)                                                     
         DC    X'FF'                                                            
*                                                                               
RPTLEVS2 DC    AL1(QMED)                                                        
         DC    AL1(QBUY)                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
* ERRORS                                                                        
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSER MVI   ERROR,X'FE'         OWN ERROR MESSAGE                            
*                                                                               
CURSOR   DS    0H                                                               
         GOTO1 CURSERR                                                          
         EJECT                                                                  
* SPOTIO INPUT HOOK                                                             
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCBL     STATION BILL                                 
         BE    BILL                                                             
         B     XIT                                                              
         EJECT                                                                  
* STATION BILL RECORD HOOK                                                      
*                                                                               
BILL     L     R2,SBAIO1                                                        
         USING STABUCKD,R2                                                      
         CLC   SVKEY(STABKMKT-STABUCKD),STABUCK  TEST ESTIMATE CHANGE           
         BE    BILL2                                                            
         LA    RE,INVTAB           YES-CLEAR THE INVOICE TABLE                  
         LA    RF,INVTABL                                                       
         XCEFL ,                                                                
*                                                                               
BILL2    MVC   SVKEY,STABUCK                                                    
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  MVC   GLOPTS+2(1),REPTYPE SET REPORT TYPE                              
         B     XIT                                                              
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
         DC    CL8'ISPTKEY ',A(ISPTKEY)                                         
         DC    CL8'OSPTKEY ',A(OSPTKEY)                                         
         DC    CL8'ITGT    ',A(ITGT)                                            
         DC    CL8'OTGT    ',A(OTGT)                                            
         DC    CL8'OSPOTS  ',A(OSPOTS)                                          
         DC    CL8'ODOL    ',A(ODOL)                                            
         DC    CL8'OTAX    ',A(OTAX)                                            
         DC    CL8'OPUR    ',A(OPUR)                                            
         DC    CL8'OAFD    ',A(OAFD)                                            
         DC    CL8'OPRINT  ',A(OPRINT)                                          
*                                                                               
         DC    CL8'IBILKEY ',A(IBILKEY)                                         
         DC    CL8'OBILKEY ',A(OBILKEY)                                         
         DC    CL8'OBILSPT ',A(OBILSPT)                                         
         DC    CL8'OBILCST ',A(OBILCST)                                         
         DC    CL8'OBILPRNT',A(OBILPRNT)                                        
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   *+8                                                              
         MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
                                                                                
ISPTKEY  LA    R5,REC              FORMAT RECORD KEY                            
         USING RECD,R5                                                          
         MVC   REC,BLANKS                                                       
         MVC   RMED,SBQMED         MEDIA                                        
         ZIC   RE,SBBEST           ESTIMATE NUMBER                              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REST,DUB                                                         
         MVC   RPRD,SBPRD          BRAND CODE                                   
         MVC   RCNTRY,ZEROS        COUNTRY CODE                                 
         LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   *+10                                                             
         MVC   RCNTRY,=C'09'                                                    
         MVC   RMKT,SBMKT          MARKET CODE                                  
         MVC   RMKTNM(L'SBMKTNM),SBMKTNM  MARKET NAME                           
         MVC   RSTA(5),SBSTA       STATION                                      
         MVI   RSTA+5,C'M'                                                      
         CLI   SBSTA+4,C' '                                                     
         BNH   *+12                                                             
         CLI   SBSTA+4,C'T'                                                     
         BNE   *+10                                                             
         MVC   RSTA+4(2),=C'TV'                                                 
         MVC   RLINE,ZEROS         BUYLINE                                      
         MVC   RMKGOOD,ZEROS       MAKEGOOD                                     
         L     R6,SBACURCH                                                      
         USING SCHUNKD,R6                                                       
         L     R3,AWEEKS           WEEK                                         
*                                                                               
ISPTKEY2 OC    0(4,R3),0(R3)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   SCDATE,0(R3)                                                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   SCDATE,2(R3)                                                     
         BNH   *+12                                                             
         LA    R3,4(R3)                                                         
         B     ISPTKEY2                                                         
         GOTO1 DATCON,DMCB,(2,(R3)),DUB                                         
         GOTO1 FMTDT,RWEEK                                                      
         GOTO1 GETESTNM            ESTIMATE START/END                           
         GOTO1 DATCON,DMCB,(2,SBESTSTP),DUB                                     
         GOTO1 FMTDT,RSTART                                                     
         GOTO1 DATCON,DMCB,(2,SBESTNDP),DUB                                     
         GOTO1 FMTDT,REND                                                       
         L     R3,SBAIO1                                                        
         USING BUYRECD,R3                                                       
         MVC   RDPT,BDDAYPT        DAYPART CODE                                 
         ZIC   RE,SBLEN            SPOT LENGTH                                  
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RSLN,DUB                                                         
         MVC   0(L'RKEY,R2),RKEY                                                
         B     XIT                                                              
         SPACE 2                                                                
OSPTKEY  LA    R5,REC              KEY OUTPUT                                   
         USING RECD,R5                                                          
         MVC   RKEY,0(R2)                                                       
         MVC   RSPOTS,ZEROS        INIT DATA FIELDS TO ZERO                     
         MVI   RSIGN,C'+'                                                       
         MVC   RCOST,ZEROS                                                      
         MVC   RTAX,ZEROS                                                       
         MVC   RPDEM,ZEROS                                                      
         MVC   RADEM,ZEROS                                                      
         MVC   RLASTMOD,ZEROS                                                   
         B     XIT                                                              
         SPACE 2                                                                
ITGT     CLC   SVTGTDEM,SBESTDEM   TARGET DEMO                                  
         BE    ITGT2                                                            
         LA    R3,DBAREA           TARGET CHANGED - GET DEMO NAME               
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVI   DBFUNCT,DBGETDEM                                                 
         MVI   DBTPTT,C'T'                                                      
         MVC   DBCOMFCS,SBCOMFAC                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         CLC   RCNTRY,=C'09'                                                    
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         GOTO1 DEMOCON,DMCB,(1,SBESTDEM),(6,TARGET),(C'S',DBLOCK),0             
*                                                                               
ITGT2    MVC   0(6,R2),TARGET                                                   
         B     XIT                                                              
         SPACE 2                                                                
OTGT     LA    R5,REC              TARGET OUTPUT                                
         USING RECD,R5                                                          
         MVC   RTARGET,0(R2)                                                    
         B     XIT                                                              
         SPACE 2                                                                
OSPOTS   L     RE,0(R2)            SPOTS                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R5,REC                                                           
         UNPK  RSPOTS,DUB                                                       
         B     XIT                                                              
         SPACE 2                                                                
ODOL     L     RE,0(R2)            COST                                         
         MVI   RSIGN,C'+'                                                       
         LTR   RE,RE                                                            
         BNM   *+10                                                             
         MVI   RSIGN,C'-'                                                       
         LPR   RE,RE                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R5,REC                                                           
         UNPK  RCOST,DUB                                                        
         B     XIT                                                              
         SPACE 2                                                                
OTAX     L     RE,0(R2)            TAX                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R5,REC                                                           
         UNPK  RTAX,DUB                                                         
         B     XIT                                                              
         SPACE 2                                                                
OPUR     L     RE,0(R2)            PURCHASED DEMO                               
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R5,REC                                                           
         UNPK  RPDEM,DUB                                                        
         SPACE 2                                                                
OAFD     L     RE,0(R2)            AFFIDAVIT DEMO                               
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R5,REC                                                           
         UNPK  RADEM,DUB                                                        
         B     XIT                                                              
         SPACE 2                                                                
OPRINT   MVC   0(126,R3),REC       PRINT THE RECORD                             
         CLI   SUPTAPE,C'Y'                                                     
         BE    XIT                                                              
         LA    R2,MSCJTAPE         WRITE A TAPE RECORD                          
         CLI   DCBOPEN,C'Y'                                                     
         BE    OPRINT2                                                          
         OPEN  ((R2),(OUTPUT))                                                  
         MVI   DCBOPEN,C'Y'                                                     
*                                                                               
OPRINT2  LA    R0,REC                                                           
         PUT   (R2),(R0)                                                        
         B     XIT                                                              
         EJECT                                                                  
* I/O ROUTINES FOR INVOICE REPORT                                               
*                                                                               
         SPACE 1                                                                
IBILKEY  LA    R5,REC              FORMAT RECORD KEY                            
         USING INVRECD,R5                                                       
         MVC   REC,BLANKS                                                       
         MVC   IMED,SBQMED         MEDIA                                        
         ZIC   RE,SBBEST           ESTIMATE NUMBER                              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  IEST,DUB                                                         
         MVC   IPRD,SBPRD          BRAND CODE                                   
         MVC   ICNTRY,ZEROS        COUNTRY CODE                                 
         LA    R1,SBAGYREC                                                      
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   *+10                                                             
         MVC   ICNTRY,=C'09'                                                    
         L     R6,SBACURCH                                                      
         USING STABELEM,R6                                                      
         SR    RE,RE                                                            
         ICM   RE,3,STABINV        INVOICE NUMBER                               
         SLL   RE,18                                                            
         SRL   RE,18                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  IINVNO+2(4),DUB                                                  
         GOTO1 DATCON,DMCB,(2,STABBDT),DUB                                      
         MVC   IINVNO(2),DUB                                                    
         MVC   ISTA(5),SBSTA       STATION                                      
         MVI   ISTA+5,C'M'                                                      
         CLI   SBSTA+4,C' '                                                     
         BNH   *+12                                                             
         CLI   SBSTA+4,C'T'                                                     
         BNE   *+10                                                             
         MVC   ISTA+4(2),=C'TV'                                                 
*Y2K         ZIC   RE,STABPER+1        BILLING PERIOD MMYYYY                    
*Y2K         CVD   RE,DUB                                                       
*Y2K         OI    DUB+7,X'0F'                                                  
*Y2K         UNPK  IPER(2),DUB                                                  
*Y2K         IC    RE,STABPER                                                   
*Y2K         CVD   RE,DUB                                                       
*Y2K         OI    DUB+7,X'0F'                                                  
*Y2K         UNPK  IPER+4(2),DUB                                                
*Y2K         MVC   IPER+2(2),=C'19'                                             
*Y2K         CLC   IPER+4(2),=C'70'                                             
*Y2K         BNL   *+10                                                         
*        THEY PROBABLY MEANT =C'20' HERE...                                     
*Y2K         MVC   IPER+2(2),=C'00'                                             
*                                                                               
         MVC   FULL(2),STABPER     SET UP FOR DATCON                            
         MVI   FULL+2,X'01'        SET DD FOR DATCON                            
         GOTO1 DATCON,DMCB,(3,FULL),(20,DUB) YYYYMMDD FORMAT                    
         MVC   IPER(2),DUB+4       MM                                           
         MVC   IPER+2(4),DUB       YYYY                                         
*                                                                               
         L     R1,ABILMNTH         WEEK START DATE - EXTRACT BILLING            
         LA    R0,NBILMNTH         PERIOD START FROM BILL MONTH TABLE           
*                                                                               
IBILKEY2 OC    0(6,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   STABPER,0(R1)                                                    
         BNE   *+14                                                             
         MVC   HALF,2(R1)          HALF=START OF BILLING PERIOD                 
         B     *+14                                                             
         LA    R1,6(R1)                                                         
         BCT   R0,IBILKEY2                                                      
         DC    H'0'                                                             
         L     R3,AWEEKS           AND FIND WHICH WEEK IT'S IN                  
*                                                                               
IBILKEY4 OC    0(4,R3),0(R3)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   HALF,0(R3)                                                       
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   HALF,2(R3)                                                       
         BNH   *+12                                                             
         LA    R3,4(R3)                                                         
         B     IBILKEY4                                                         
         GOTO1 DATCON,DMCB,(2,(R3)),DUB                                         
         GOTO1 FMTDT,IWEEK                                                      
         GOTO1 GETESTNM            ESTIMATE START/END                           
         GOTO1 DATCON,DMCB,(2,SBESTSTP),DUB                                     
         GOTO1 FMTDT,ISTART                                                     
         GOTO1 DATCON,DMCB,(2,SBESTNDP),DUB                                     
         GOTO1 FMTDT,IEND                                                       
         BAS   RE,GETINV           GET INVOICE                                  
         MVC   IINVDT,SVINVDT      INVOICE DATE                                 
         MVC   IDUEDT,SVDUEDT      DUE DATE                                     
         MVC   0(L'IKEY,R2),IKEY                                                
         B     XIT                                                              
         SPACE 2                                                                
OBILKEY  LA    R5,REC              KEY OUTPUT                                   
         USING INVRECD,R5                                                       
         MVC   IKEY,0(R2)                                                       
         MVC   ISPOTS,ZEROS        INIT DATA FIELDS TO ZERO                     
         MVI   ISIGN,C'+'                                                       
         MVC   ICOST,ZEROS                                                      
         MVC   ILASTMOD,ZEROS                                                   
         B     XIT                                                              
         SPACE 2                                                                
OBILSPT  L     RE,0(R2)            BILLED SPOTS                                 
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R5,REC                                                           
         UNPK  ISPOTS,DUB                                                       
         B     XIT                                                              
         SPACE 2                                                                
OBILCST  L     RE,0(R2)            BILL COST                                    
         MVI   ISIGN,C'+'                                                       
         LTR   RE,RE                                                            
         BNM   *+10                                                             
         MVI   ISIGN,C'-'                                                       
         LPR   RE,RE                                                            
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R5,REC                                                           
         UNPK  ICOST,DUB                                                        
         B     XIT                                                              
         SPACE 2                                                                
OBILPRNT MVC   0(91,R3),REC        PRINT THE RECORD                             
         CLI   SUPTAPE,C'Y'                                                     
         BE    XIT                                                              
         LA    R2,ISCJTAPE         WRITE A TAPE RECORD                          
         CLI   DCBOPEN,C'Y'                                                     
         BE    OBILP2                                                           
         OPEN  ((R2),(OUTPUT))                                                  
         MVI   DCBOPEN,C'Y'                                                     
*                                                                               
OBILP2   LA    R0,REC                                                           
         PUT   (R2),(R0)                                                        
         B     XIT                                                              
         EJECT                                                                  
* GET INVOICE DETAILS FROM BILL HEADER RECORD                                   
* ROUTINE MAINTAINS A TABLE OF BILL HEADER INFO AND RETURNS:                    
* SVINVDT = INVOICE DATE                                                        
* SVDUEDT = DUE DATE                                                            
*                                                                               
GETINV   NTR1  ,                                                                
         L     R6,SBACURCH                                                      
         USING STABELEM,R6                                                      
         GOTO1 DATCON,DMCB,(2,STABBDT),(3,FULL)  FULL(3)=BILLING DATE           
         SR    RE,RE                                                            
         ICM   RE,3,STABINV                                                     
         SLL   RE,18                                                            
         SRL   RE,18                                                            
         STH   RE,HALF             HALF=INVOICE NUMBER                          
         LA    R5,INVTAB                                                        
         USING INVD,R5                                                          
         LA    R0,INVMAX           SEARCH INVOICE TABLE FOR BILLING             
*                                  PERIOD, BILL MONTH AND INV NUM MATCH         
GETINV2  OC    0(INVL,R5),0(R5)                                                 
         BZ    GETINV6                                                          
         CLC   INVPER,STABPER                                                   
         BNE   GETINV4                                                          
         CLC   INVMON,FULL+1                                                    
         BNE   GETINV4                                                          
         CLC   INVNUM,HALF                                                      
         BE    GETINV8             FOUND                                        
*                                                                               
GETINV4  LA    R5,INVL(R5)                                                      
         BCT   R0,GETINV2                                                       
*                                                                               
GETINV6  XC    KEY,KEY             NOT FOUND - READ BILL HEADER                 
         LA    R2,KEY                                                           
         USING BILLRECD,R2                                                      
         MVC   BKEYAM,SBBAGYMD                                                  
         MVC   BKEYCLT,SBBCLT                                                   
         MVC   BKEYPRD,SBPRD                                                    
         MVC   BKEYEST,SBBEST                                                   
         MVC   BKEYYSRV,STABPER                                                 
         MVC   BKEYMSRV,STABPER+1                                               
         MVC   BYTE,FULL                                                        
         NI    BYTE,X'0F'                                                       
         ZIC   RE,BYTE                                                          
         CLI   BYTE,10                                                          
         BL    *+8                                                              
         SH    RE,=H'10'                                                        
         SLL   RE,4                                                             
         ZIC   RF,FULL+1                                                        
         OR    RE,RF                                                            
         STC   RE,BKEYMBIL                                                      
         MVC   BKEYINV,HALF                                                     
         GOTO1 HIGH                                                             
         CLC   BKEY,KEYSAVE                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,SBAIO3                                                        
         ST    R2,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   INVPER(1),BKEYYSRV  NEW TABLE ENTRY                              
         MVC   INVPER+1(1),BKEYMSRV                                             
         MVC   INVMON,BKEYMBIL                                                  
         MVC   INVNUM,BKEYINV                                                   
         MVC   DUB(6),BQDATE       INVOICE DATE                                 
         GOTO1 FMTDT,INVDTE                                                     
         GOTO1 DATCON,DMCB,(3,BDUEDATE),DUB  DUE DATE                           
         GOTO1 FMTDT,INVDUEDT                                                   
*                                                                               
GETINV8  MVC   SVINVDT,INVDTE                                                   
         MVC   SVDUEDT,INVDUEDT                                                 
*                                                                               
GETINVX  B     XIT                                                              
         B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO FORMAT A DATE IN MMDDYYYY FORMAT                                   
* DUB = YYMMDD                                                                  
*                                                                               
FMTDT    NTR1                                                                   
*Y2K         MVC   0(4,R1),DUB+2                                                
*Y2K         MVC   4(2,R1),=C'19'                                               
*Y2K         MVC   6(2,R1),DUB                                                  
*Y2K         CLC   DUB(2),=C'70'                                                
*Y2K         BNL   *+10                                                         
*Y2K         MVC   4(2,R1),=C'20'                                               
*Y2K         BR    RE                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(20,WORK) YYYYMMDD FORMAT                    
         MVC   0(4,R1),WORK+4      MMDD                                         
         MVC   4(4,R1),WORK        YYYY                                         
         XIT1                                                                   
         EJECT                                                                  
* FINAL CALL FOR REQUEST                                                        
*                                                                               
FINAL    L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL                                                      
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
* RUN LAST                                                                      
*                                                                               
RUNLST   L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         CLI   DCBOPEN,C'Y'                                                     
         BNE   XIT                                                              
         LA    R2,MSCJTAPE         CLOSE THE TAPE                               
         CLI   REPTYPE,C'I'                                                     
         BNE   *+8                                                              
         LA    R2,ISCJTAPE                                                      
         CLOSE ((R2),)                                                          
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
ASAVE    DS    A                                                                
*                                                                               
SUPTAPE  DS    CL1                                                              
SVINVDT  DS    CL8                                                              
SVDUEDT  DS    CL8                                                              
SVTGTDEM DS    XL3                                                              
SVKEY    DS    XL13                                                             
TARGET   DS    XL3                                                              
REC      DS    CL126                                                            
*                                                                               
INVTAB   DS    (INVMAX)CL(INVL)                                                 
INVTABL  EQU   *-INVTAB                                                         
INVMAX   EQU   50                                                               
*                                                                               
ZEROS    DC    CL12'000000000000'                                               
BLANKS   DC    CL256' '                                                         
*                                                                               
DBAREA   DS    CL256                                                            
         SPACE 2                                                                
* VALUES SAVED BETWEEN REQUESTS                                                 
*                                                                               
SAVVALS  DS    0X                                                               
*                                                                               
REPTYPE  DS    CL1                                                              
DCBOPEN  DS    CL1                                                              
MSCJTAPE DCB   DDNAME=SCJTAPE,DSORG=PS,RECFM=FB,LRECL=126,BLKSIZE=1260,X        
               MACRF=PM                                                         
ISCJTAPE DCB   DDNAME=SCJTAPE,DSORG=PS,RECFM=FB,LRECL=91,BLKSIZE=9100, X        
               MACRF=PM                                                         
*                                                                               
SAVVALSL EQU   *-SAVVALS                                                        
         EJECT                                                                  
* INVOICE TABLE DSECT                                                           
*                                                                               
INVD     DSECT                                                                  
INVPER   DS    XL2                 BILLING PERIOD Y/M                           
INVMON   DS    XL1                 BILL MONTH                                   
INVNUM   DS    XL2                 INVOICE NUMBER                               
INVDTE   DS    CL8                 INVOICE DATE                                 
INVDUEDT DS    XL8                 DUE DATE                                     
INVL     EQU   *-INVD                                                           
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
* TAPE RECORD DSECTS                                                            
*                                                                               
         SPACE 1                                                                
RECD     DSECT                     SPOT MEDIA TAPE                              
*                                                                               
RKEY     DS    0CL85                                                            
RMED     DS    CL1                                                              
REST     DS    CL4                                                              
RPRD     DS    CL2                                                              
RCNTRY   DS    CL2                                                              
RMKT     DS    CL4                                                              
         DS    CL2                                                              
RMKTNM   DS    CL30                                                             
RSTA     DS    CL6                                                              
RLINE    DS    CL3                                                              
RMKGOOD  DS    CL2                                                              
RWEEK    DS    CL8                                                              
RSTART   DS    CL8                                                              
REND     DS    CL8                                                              
RDPT     DS    CL1                                                              
         DS    CL1                                                              
RSLN     DS    CL3                                                              
*                                                                               
RDATA    DS    0CL41                                                            
RSPOTS   DS    CL2                                                              
RSIGN    DS    CL1                                                              
RCOST    DS    CL8                                                              
RTAX     DS    CL8                                                              
RTARGET  DS    CL6                                                              
RPDEM    DS    CL4                                                              
RADEM    DS    CL4                                                              
RLASTMOD DS    CL8                                                              
         SPACE 2                                                                
INVRECD  DSECT                     SPOT INVOICE TAPE                            
*                                                                               
IKEY     DS    0CL67                                                            
IMED     DS    CL1                                                              
IEST     DS    CL4                                                              
IPRD     DS    CL2                                                              
ICNTRY   DS    CL2                                                              
IINVNO   DS    CL6                                                              
ISTA     DS    CL6                                                              
IPER     DS    CL6                                                              
IWEEK    DS    CL8                                                              
ISTART   DS    CL8                                                              
IEND     DS    CL8                                                              
IINVDT   DS    CL8                                                              
IDUEDT   DS    CL8                                                              
*                                                                               
IDATA    DS    0CL24                                                            
ISPOTS   DS    CL5                                                              
ISIGN    DS    CL1                                                              
ICOST    DS    CL10                                                             
ILASTMOD DS    CL8                                                              
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*DEDBLOCK                                                                       
*SPGENAGY                                                                       
*SPGENBUY                                                                       
*SPGENBILL                                                                      
*SPGENSTAB                                                                      
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRID8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIDAD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPWRI28   12/16/04'                                      
         END                                                                    
