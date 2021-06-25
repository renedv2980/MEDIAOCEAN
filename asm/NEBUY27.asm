*          DATA SET NEBUY27    AT LEVEL 011 AS OF 12/13/10                      
*PHASE T31127A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - TRANSFER UNITS OVERLAY - T31127'           
T31127   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TRAN**,RA,RR=RE                                              
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL STORAGE                   
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R5                                                       
         SPACE                                                                  
COPY     TM    MODE,DISPLAY        TEST FOR CHANGE IN HEADLINE FIELDS           
         BZ    *+8                 NO                                           
         OI    MODE,FIRST          YES-START CPY OVER (TO DETS. CHGD.)          
         TM    MODE,FIRST          TEST FOR FIRST TIME                          
         BZ    IC100               NO                                           
         BAS   RE,CLRSAVE          CLEAR SAVE AREA                              
         SPACE                                                                  
* HEADLINE VALIDATION SAVES                                                     
*                                                                               
IC100    MVC   FRSTTYPE,NBSTSTAT   FROM STATION TYPE                            
         MVC   FRPTTYPE,NBSTPSTT   FROM POSTING TYPE                            
         MVC   FRSUBMED,NBSUBMED   SUB MEDIA CODE                               
         MVI   DELKEY,X'FF'                                                     
         SPACE                                                                  
* EDIT FROM CLIENT                                                              
*                                                                               
         LA    R2,COPFCLTH                                                      
         GOTO1 VGETFLD                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FERN,INVERR                                                      
         GOTO1 VCLPACK,DMCB,FLD,FRCLT                                           
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    ERROR                                                            
         CLC   FRCLT,SVFRCLT       TEST FOR CHANGE IN FROM CLIENT               
         BE    *+8                                                              
         OI    MODE,FIRST                                                       
         MVC   SVFRCLT,FRCLT       UPDATE LAST CLIENT                           
         SPACE                                                                  
* EDIT FROM ESTIMATE                                                            
*                                                                               
IC200    LA    R2,COPFESTH                                                      
         GOTO1 VGETFLD                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FERN,NUMERR                                                      
         TM    FLDH+4,X'08'                                                     
         BZ    ERROR                                                            
         MVI   FERN,INVERR                                                      
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,FREST                                                         
         CLC   FREST,SVFREST                                                    
         BE    *+8                                                              
         OI    MODE,FIRST                                                       
         MVC   SVFREST,FREST                                                    
         SPACE                                                                  
* EDIT FROM PACKAGE                                                             
*                                                                               
IC300    LA    R2,COPFRPKH                                                      
         GOTO1 VGETFLD                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FERN,NUMERR                                                      
         TM    FLDH+4,X'08'                                                     
         BZ    ERROR                                                            
         MVI   FERN,INVERR                                                      
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,FRPACK                                                        
         CLC   FRPACK,SVFRPACK     TEST FOR CHANGE IN FROM PACKAGE              
         BE    *+8                                                              
         OI    MODE,FIRST                                                       
         MVC   SVFRPACK,FRPACK                                                  
         SPACE                                                                  
* EDIT PRODUCT FILTER                                                           
*                                                                               
IC400    LA    R2,COPFRPRH                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    IC420               NO INPUT                                     
         CLI   FLDH+5,2            PRODUCT MUST BE 2-3 LETTERS                  
         BL    ERROR                                                            
         TM    FLDH+4,X'04'        TEST ALPHA DATA                              
         BZ    ERROR                                                            
         MVC   FRPRD,FLD                                                        
*                                                                               
IC420    CLC   FRPRD,SVFRPRD       TEST FOR CHANGE IN FROM PRODUCT              
         BE    *+8                                                              
         OI    MODE,FIRST                                                       
         MVC   SVFRPRD,FRPRD                                                    
         SPACE                                                                  
* EDIT UNIT FILTER                                                              
*                                                                               
IC440    LA    R2,COPFRU1H                                                      
         LA    R3,UNITLST                                                       
         LA    R4,15                                                            
         MVI   FERN,INVERR                                                      
IC450    GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    IC470               NO INPUT                                     
*--VALIDATE AND COMPRESS DATE                                                   
         GOTO1 VSCANNER,DMCB,(R2),(1,WORK),C',=,-'                              
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         GOTO1 VDATVAL,DMCB,(0,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,DUB),(2,(R3))                                    
*--VALIDATE AND CONVERT THE LINE NUMBER                                         
         MVI   2(R3),1             DEFAULT LINE NUMBER                          
         CLI   WORK+1,0                                                         
         BE    IC460                                                            
         TM    WORK+3,X'80'                                                     
         BZ    ERROR                                                            
         MVC   2(1,R3),WORK+11                                                  
IC460    LA    R3,3(R3)                                                         
*--BUMP TO NEXT FIELD                                                           
IC470    ZIC   R1,FLDH                                                          
         AR    R2,R1                                                            
         BCT   R4,IC450                                                         
         MVI   0(R3),X'FF'                                                      
         SPACE                                                                  
* EDIT PRODUCT ALLOCATION                                                       
*                                                                               
IC500    LA    R2,COPFRPAH                                                      
         XC    ALPRD,ALPRD                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    IC600               NO INPUT                                     
         MVI   FERN,INVERR                                                      
         CLI   FLD,NO              VALIDATE NO                                  
         BE    IC600                                                            
         CLI   FLD,YES             VALIDATE YES                                 
         BNE   ERROR                                                            
         CLC   BUYCLI,COPFCLT                                                   
         BNE   ERROR                                                            
         MVC   ALPRD,FLD                                                        
         SPACE 1                                                                
* EDIT TO PACKAGE                                                               
*                                                                               
IC600    LA    R2,COPTOPKH                                                      
         GOTO1 VGETFLD                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FERN,NUMERR                                                      
         TM    FLDH+4,X'08'        TEST FOR NUMERIC FIELD                       
         BZ    ERROR                                                            
         MVI   FERN,INVERR                                                      
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,TOPACK                                                        
         CLC   FRCLT,CLIPK         TEST FOR SAME CLIENT                         
         BNE   IC620               NO                                           
         CLC   FREST,EST           AND SAME ESTIMATE                            
         BNE   IC620                                                            
         CLC   FRPACK,TOPACK       TEST FROM/TO PACKAGE SAME                    
         BE    ERROR               STOP THIS COPY                               
*                                                                               
IC620    CLC   TOPACK,SVTOPACK                                                  
         BE    *+8                                                              
         OI    MODE,FIRST                                                       
         MVC   SVTOPACK,TOPACK                                                  
         SPACE                                                                  
* VALIDATE TO PACKAGE FIRST                                                     
*                                                                               
IC700    MVC   NBSELPAK,TOPACK     READ THE TO PACKAGE                          
         MVI   NBSELMOD,NBPROCPK                                                
         MVI   NBSELPST,C'B'                                                    
         MVI   NBDATA,C'P'                                                      
         MVC   NBAIO,AIOAREA1                                                   
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+12                                                             
         MVI   FERN,PAKERR                                                      
         B     ERROR                                                            
*                                                                               
         L     R4,NBAIO                                                         
         USING NPRECD,R4                                                        
         MVI   FERN,AUDACTER                                                    
         TM    NPAKSTAT,X'02'      IS PACKAGE AUDIT SET                         
         BO    ERROR               YES, CANT DO COPY                            
         MVI   FERN,PAKLERR                                                     
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BO    ERROR                                                            
         MVI   FERN,PAKFERR        TEST FOR FROZEN PACKAGE                      
         TM    NPAKSTAT,X'80'                                                   
         BO    ERROR                                                            
         BAS   RE,CHKREASN         CHECK REASON CODE                            
*                                                                               
         XC    COPTPKD,COPTPKD                                                  
         OI    COPTPKDH+6,X'80'                                                 
         MVC   COPTPKD(L'NBPAKNAM),NBPAKNAM                                     
         MVC   COPTPKD+L'NBPAKNAM+1(L'NBDPNAM),NBDPNAM                          
*                                                                               
         LA    RE,PACKREC          MOVE THE TO PACKAGE TO DESIGNATED            
         ST    RE,APACKREC         AREA                                         
         LR    R0,R4                                                            
         LH    R1,NPKRLEN                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         MVC   PACK,TOPACK                                                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',PACKREC),(1,=C'K')             
         CLI   12(R1),0                                                         
         BNE   IC800                                                            
         MVC   APAKFEL,12(R1)      SET PACKAGE FILTER ADCON                     
         SPACE                                                                  
* VALIDATE FROM AGENCY/CLIENT                                                   
*                                                                               
IC800    TM    MODE,FIRST          TEST FOR FIRST TIME MODE                     
         BZ    CP100               NO-NO NEED TO VALIDATE FROM DATA             
*                                                                               
         GOTO1 VBLDRQST                                                         
*                                                                               
         LA    RE,NEBLOCKA         CLEAR THE NETBLOCK                           
         LA    RF,NEBLOCKL                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   NBSELAGY,AGYALPH                                                 
         MVI   NBSELMED,C'N'                                                    
         MVI   NBSELMOD,NBVALAGY                                                
         MVC   NBAIO,AIOAREA1                                                   
         MVC   NBACOM,ACOMFACS                                                  
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,COPFCLTH                                                      
         ST    R2,FADDR            SET ERROR CURSOR POSITION                    
         MVC   NBSELCL2,FRCLT                                                   
         MVI   NBSELMOD,NBVALCLI                                                
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+14                                                             
         MVC   FERN,NBERROR                                                     
         B     ERROR                                                            
         L     R4,NBAIO                                                         
         USING CLTHDRD,R4                                                       
         MVC   COPFCLD,CNAME                                                    
         OI    COPFCLDH+6,X'80'                                                 
*                                                                               
         OC    TWAACCS(2),TWAACCS  TEST FOR SECURITY LIMITS                     
         BZ    IC900                                                            
         CLI   TWAACCS,C'+'        TEST FOR MARKET LOCKET                       
         BE    IC900               NO                                           
         MVI   FERN,SCTYERR                                                     
         CLI   TWAACCS,C'*'        TEST FOR OFFICE LIMIT                        
         BE    *+18                                                             
         CLC   TWAACCS(2),FRCLT    TEST FOR FILTERED CLIENT                     
         BE    IC900               OK                                           
         B     ERROR                                                            
*                                                                               
         CLC   TWAACCS+1(1),COFFICE TEST FOR FILTERED OFFICE                    
         BNE   ERROR                                                            
         SPACE                                                                  
* VALIDATE FROM PRODUCT                                                         
*                                                                               
IC900    MVI   SVFRPRN,0           INITIALIZE LAST PRODUCT NUMBER               
         OC    FRPRD,FRPRD         TEST FOR PRODUCT FILTER                      
         BZ    IC920                                                            
         LA    R2,COPFRPRH                                                      
         ST    R2,FADDR            SET CURSOR ERROR POSITION                    
         BAS   RE,VALPRD           VALIDATE CODE                                
         MVC   SVFRPRN,FRPRN       SAVE THIS PRODUCT NUMBER                     
         SPACE 1                                                                
* VALIDATE FROM ESTIMATE                                                        
*                                                                               
IC920    LA    R2,COPFESTH                                                      
         ST    R2,FADDR                                                         
         MVC   NBSELEST,FREST                                                   
         MVI   NBSELMOD,NBVALEST                                                
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+12                                                             
         MVI   FERN,ESTERR                                                      
         B     ERROR                                                            
         L     R4,NBAIO                                                         
         USING ESTHDRD,R4                                                       
         MVI   FERN,ELOCKERR       TEST FOR LOCKED ESTIMATE                     
         TM    ECNTRL,X'08'                                                     
         BO    ERROR                                                            
         XC    COPFESD,COPFESD                                                  
         OI    COPFESDH+6,X'80'                                                 
         MVC   COPFESD(20),EDESC   ESTIMATE NAME                                
         GOTO1 VDATCON,DMCB,ESTART,(5,COPFESD+22)                               
         MVI   COPFESD+30,DASH                                                  
         GOTO1 (RF),(R1),EEND,(5,COPFESD+31)                                    
*                                                                               
         MVC   NBSELNET,NET        SET NETWORK                                  
         SPACE                                                                  
* VALIDATE FROM PACKAGE                                                         
*                                                                               
IC1000   LA    R2,COPFRPKH                                                      
         ST    R2,FADDR            SET ERROR CURSOR POSITION                    
         MVC   NBSELPAK,FRPACK                                                  
         MVI   NBSELMOD,NBPROCPK                                                
         MVI   NBSELPST,C'B'                                                    
         MVI   NBDATA,C'P'                                                      
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+12                                                             
         MVI   FERN,PAKERR                                                      
         B     ERROR                                                            
*                                                                               
         MVI   FERN,PAKLERR                                                     
         L     R4,NBAIO                                                         
         USING NPRECD,R4                                                        
         MVI   FERN,AUDACTER                                                    
         TM    NPAKSTAT,X'02'      IS PACKAGE AUDIT SET                         
         BO    ERROR               YES, CANT DO COPY                            
         TM    NPAKSTAT,X'20'                                                   
         BO    ERROR                                                            
         MVI   FERN,PAKFERR                                                     
         TM    NPAKSTAT,X'80'                                                   
         BO    ERROR                                                            
*                                                                               
         XC    COPFPKD,COPFPKD     OUTPUT THE PACKAGE DESCRIPTION               
         OI    COPFPKDH+6,X'80'                                                 
         MVC   COPFPKD(L'NBPAKNAM),NBPAKNAM                                     
         MVC   COPFPKD+L'NBPAKNAM+1(L'NBDPNAM),NBDPNAM                          
         EJECT                                                                  
* FIRST TIME CODE                                                               
*                                                                               
CP100    TM    MODE,FIRST          TEST FOR FIRST TIME                          
         BZ    CP300                                                            
*                                                                               
         MVC   NBSELDP,NBACTDP                                                  
         MVC   NBSELSTR,ESTSTART                                                
         MVC   NBSELEND,ESTEND                                                  
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'Q'          PROGRAM SEQUENCE                             
         MVI   NBSELMOD,NBPROCUN                                                
*                                                                               
         LA    RE,SVNBLOCK         SAVE THE NETBLOCK FOR RETURN                 
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LR    R0,R6                                                            
         MVCL  RE,R0                                                            
         XC    SVUNITS,SVUNITS                                                  
         XC    SVPRDLST,SVPRDLST                                                
*                                                                               
CP200    GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR A UNIT RECORD                       
         BE    EC100                                                            
         SPACE                                                                  
         CLI   NBMODE,NBREQLST                                                  
         BNE   CP200                                                            
         MVC   BUYMSG(L'NOUNITS),NOUNITS                                        
         MVI   MODE,0                                                           
         BAS   RE,CLRSAVE          CLEAR SAVE AREA                              
         LA    R2,COPFRPKH                                                      
         ST    R2,FADDR                                                         
         GOTO1 VEXIT                                                            
         SPACE 2                                                                
* RESTORE SAVED NETBLOCK AND EDIT COPY FIELD                                    
*                                                                               
CP300    LR    RE,R6               BEGIN WITH SAVED NETBLOCK                    
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,SVNBLOCK                                                      
         MVCL  RE,R0                                                            
         MVC   FRPRN,SVFRPRN       RESTORE SAVED PRODUCT NUMBER                 
         MVI   FRSTSW,YES          SET FIRST RECORD READ SWITCH                 
         MVC   NBAIO,AIOAREA1      REFRESH BLOCK ADDRESSES                      
         MVC   NBACOM,ACOMFACS                                                  
         MVI   NBFUNCT,NBFRDHI     FORCE READ HIGH TO RESET SEQUENCE            
*                                                                               
         LA    R2,COPYESH          EDIT COPY FIELD                              
         GOTO1 VGETFLD                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         MVI   FERN,INVERR                                                      
         MVC   COPYSW,FLD                                                       
         EX    R1,YESCOMP                                                       
         BE    *+12                                                             
         EX    R1,NOCOMP                                                        
         BNE   ERROR                                                            
         SPACE                                                                  
CP320    CLI   ALPRD,YES                                                        
         BNE   CP400                                                            
         BAS   RE,CHKPRDES         CHECK PRODS EXIST FOR ESTIMATE               
         SPACE                                                                  
* LOOP PROCESSING - SAVE NETBLOCK, READ RECORD, TEST IF QUALIFIED               
* FOR COPY, AND COPY IT                                                         
*                                                                               
CP400    LA    RE,SVNBLOCK         SAVE THE NETBLOCK BEFORE EACH READ           
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LR    R0,R6                                                            
         MVCL  RE,R0                                                            
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    DEL100              YES                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   CP400                                                            
         CLI   FRSTSW,YES          TEST FOR RE-READ OF FIRST RECORD             
         BE    CP500               YES-SKIP PROGRAM BREAK CHECK                 
         TM    NBSUBMSK,NBSBMPRG   TEST FOR CHANGE IN PROGRAM                   
         BO    DEL100              YES                                          
         SPACE                                                                  
CP500    MVI   FRSTSW,NO                                                        
         CLI   COPYSW,NO           TEST IF COPYING                              
         BE    CP400               NO-READ UNTIL EOF OR CONTROL BREAK           
         OC    FRPRD,FRPRD         TEST FOR PRODUCT FILTER                      
         BZ    *+14                NO                                           
         CLC   FRPRN,NBPRD         FILTER ON FIRST PRODUCT                      
         BNE   CP400               NO-NEXT RECORD                               
*                                                                               
         LA    R2,COPFRU1H                                                      
         LA    RE,UNITLST                                                       
         LA    RF,15                                                            
CP520    CLI   0(RE),X'FF'                                                      
         BE    CP400                                                            
         CLC   0(2,RE),NBACTDAT                                                 
         BNE   CP530                                                            
         CLC   2(1,RE),NBACTSUB                                                 
         BE    CP560                                                            
CP530    LA    RE,3(RE)            NEXT TABLE ENTRY                             
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT SCREEN FIELD                            
         BCT   RF,CP520                                                         
         B     CP400                                                            
*                                                                               
CP560    L     RF,NBAIO            POINT TO RECORD                              
         LA    RF,NUMAINEL-NUKEY(RF)  FIRST ELEMENT                             
         SPACE                                                                  
*--   TEST FOR MISSING/MAKE-GOOD/BILL/PAY                                       
CP600    CLI   0(RF),0             TEST FOR EOR                                 
         BE    CP650                                                            
         CLI   0(RF),X'06'                                                      
         BE    MKGER                                                            
         CLI   0(RF),X'07'                                                      
         BE    MDGER                                                            
         CLI   0(RF),X'10'                                                      
         BE    BILLER                                                           
         CLI   0(RF),X'12'                                                      
         BE    PAIDER                                                           
         ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     CP600                                                            
*                                                                               
CP650    DS    0H                                                               
         XC    DMWORK(96),DMWORK                                                
         LA    RE,DMWORK                                                        
         STCM  RE,15,NBABILRD       ADDRESS OF BILL DSECT IN NETBLOCK           
         USING NBLBILLD,RE                                                      
         MVC   NBLUNAIO,NBAIO                                                   
         OI    NBLFUNC,NBLBLD                                                   
         GOTO1 VBILLRDR,DMCB,NETBLOCK,RR=MYRELO                                 
         LA    RE,DMWORK                                                        
         TM    NBLFUNC,NBLBILD                                                  
         BO    BILLER                                                           
*                                                                               
CP700    L     R1,NBAIO                                                         
         USING NURECD,R1                                                        
         TM    NUUNITST,X'40'      TEST FOR PRE-EMPT                            
         BNZ   CP400                                                            
*                                                                               
*--SAVE KEY TO BE DELETED                                                       
         LA    RE,DELKEY                                                        
         LA    RF,15                                                            
CP800    CLI   0(RE),X'FF'                                                      
         BE    CP820                                                            
         LA    RE,20(RE)                                                        
         BCT   RF,CP800                                                         
         DC    H'0'                                                             
CP820    MVC   0(20,RE),NUKEY      SAVE KEY TO BE DELETED                       
         MVI   20(RE),X'FF'        MOVE END OF TABLE                            
         DROP  R1                                                               
         SPACE                                                                  
         MVC   PPROG,SVPROG        PROGRAM PERIMATER                            
         BAS   RE,BUILD            CONSTRUCT RECORD/WRITE TO FILE               
         B     CP400               GET NEXT RECORD                              
*--DELETE THE COPIED UNITS                                                      
DEL100   BAS   RE,DELETE                                                        
         B     EC100                                                            
         SPACE                                                                  
MDGER    MVC   BUYMSG(L'MDGERR),MDGERR                                          
         B     ERROUT                                                           
*                                                                               
MKGER    MVC   BUYMSG(L'MKGERR),MKGERR                                          
         B     ERROUT                                                           
*                                                                               
BILLER   MVC   BUYMSG(L'BILLERR),BILLERR                                        
         B     ERROUT                                                           
*                                                                               
PAIDER   MVC   BUYMSG(L'PAIDERR),PAIDERR                                        
         B     ERROUT                                                           
*                                                                               
ERROUT   MVI   MODE,0                                                           
         BAS   RE,CLRSAVE          CLEAR SAVE AREA                              
         ST    R2,FADDR                                                         
         GOTO1 VEXIT                                                            
         EJECT                                                                  
* EOF/NEW PROGRAM PROCESSING                                                    
*                                                                               
EC100    XC    COPPRDS,COPPRDS     CLEAR PROGRAM DESCRIPTION                    
         OI    COPPRDSH+6,X'80'    XMIT                                         
         XC    COPSUM,COPSUM       CLEAR SUMMARY LINE                           
         OI    COPSUMH+6,X'80'                                                  
*--CLEAR UNIT EDIT                                                              
         LA    RE,COPFRU1H                                                      
         LA    RF,15                                                            
EC150    XC    8(12,RE),8(RE)      CLEAR FIELD                                  
         OI    6(RE),X'80'         TRANSMIT                                     
         ZIC   R1,0(RE)                                                         
         AR    RE,R1               GET NEXT FIELD                               
         BCT   RF,EC150                                                         
*                                                                               
         L     R3,PRGUNITS                                                      
         TM    MODE,FIRST                                                       
         BO    EC200                                                            
         LA    R2,COPSUM                                                        
         EDIT  (R3),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0               UPDATE OUTPUT POINTER                        
         MVC   1(16,R2),=C'UNITS COPIED FOR'                                    
         LA    R2,18(R2)                                                        
         MVC   0(L'SVPROG,R2),SVPROG    LAST PROGRAM                            
         SPACE                                                                  
EC200    A     R3,SVUNITS          UPDATE COUNT OF COPIED UNITS                 
         ST    R3,SVUNITS                                                       
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    EC400                                                            
         MVC   PROG,NBACTPRG                                                    
         MVC   SVPROG,PROG         GET PROGRAM                                  
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         CLI   FERN,0                                                           
         BNE   EC300                                                            
*                                                                               
         MVC   COPPRDS(L'PROG),PROG                                             
         LA    R0,L'PROG                                                        
         LA    R2,COPPRDS+L'PROG-1                                              
         CLI   0(R2),C' '          FIND LAST CHARACTER OF PROGRAM CODE          
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   R0,*-10                                                          
         LA    R2,1(R2)                                                         
         MVI   0(R2),SLASH                                                      
         LA    R2,1(R2)                                                         
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   0(L'NPGNAME,R2),NPGNAME                                          
         MVC   SVPGDYTM,NPGDAY     SAVE DAY TIME                                
         DROP  RE                                                               
         SPACE                                                                  
EC300    LA    R2,COPYESH                                                       
         ST    R2,FADDR                                                         
         MVC   8(3,R2),=C'YES'     XMIT 'YES' IN COPY                           
         OI    6(R2),X'81'         MODIFIED NEXT TIME                           
         MVC   BUYMSG(4),=C'NEXT'                                               
         LA    R3,BUYMSG+5                                                      
         TM    MODE,FIRST                                                       
         BZ    *+14                                                             
         MVC   BUYMSG(5),=C'FIRST'                                              
         LA    R3,BUYMSG+6                                                      
         MVC   0(13,R3),=C'PROGRAM SHOWN'                                       
         LA    R3,14(R3)                                                        
         MVI   0(R3),DASH                                                       
         LA    R3,2(R3)                                                         
         MVC   0(19,R3),=C'ENTER ''YES'' TO COPY'                               
         XC    PRGUNITS,PRGUNITS                                                
         LA    R2,COPFRU1H                                                      
         ST    R2,FADDR                                                         
         B     COPYX                                                            
         SPACE                                                                  
EC400    EDIT  (R3),(4,COPSUM),ALIGN=LEFT                                       
         LA    R2,COPSUM                                                        
         AR    R2,R0                                                            
         MVC   1(24,R2),=C'UNITS COPIED FOR PACKAGE'                            
         LA    R2,BUYACTH          SET CURSOR POSITION                          
         ST    R2,FADDR                                                         
         MVC   BUYMSG(L'EOFMSG),EOFMSG                                          
         BAS   RE,CLRSAVE                                                       
         SPACE                                                                  
COPYX    NI    MODE,X'FF'-DISPLAY                                               
         B     EXXMOD                                                           
         SPACE 2                                                                
YESCOMP  CLC   FLD(0),=C'YES'                                                   
NOCOMP   CLC   FLD(0),=C'NO '                                                   
         EJECT                                                                  
* SUB-ROUTINE TO PREPARE UNIT TO BE COPIED AND TO ADD TO FILE                   
* PPROG = PROGRAM TO BE ADDED                                                   
*                                                                               
BUILD    NTR1                                                                   
         OC    VEDIT,VEDIT                                                      
         BNZ   BD100                                                            
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,0(R1)         GENERAL EDIT MODULE                          
         SPACE                                                                  
BD100    LA    R3,BLOCK                                                         
         USING UNBLOCKD,R3                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,NBAIO                                                     
         LA    R1,TEMPD+1500                                                    
         ST    R1,UNALOCAL                                                      
         L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
         MVC   NUKCLT,CLIPK        SET TO CLIENT                                
         MVC   NUKEST,EST          TO ESTIMATE                                  
         MVC   NUKPROG,PPROG                                                    
         MVC   NUPACK,TOPACK                                                    
         NI    NUACTWHY,X'04'      KEEP PFB ON - TURN OFF THE REST              
         OI    NUACTWHY,X'81'      SET FOR NEW BUY/COPY                         
         BAS   RE,GETSUB           GET NEW SUB-LINE                             
*                                                                               
         ICM   RE,15,APROGEL                                                    
         BZ    *+10                                                             
         USING NPGELEM,RE                                                       
         MVC   NUPROGNM,NPGNAME                                                 
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUKDP,NPAKDP                                                     
*                                                                               
         CLC   CLIPK,FRCLT         IF COPYING TO ANOTHER CLIENT                 
         BNE   BD120                 DONT CARRY PRODUCTS                        
         CLC   EST(1),FREST        OR BETWEEN ESTS. W/IN CLIENT                 
         BE    BD140                                                            
         CLI   ALPRD,YES           ALLOCATE PRODUCTS                            
         BE    BD140                                                            
BD120    MVC   NUPRD,NPAKMAST      USE PACKAGE MASTER ALLOCATION                
         MVI   NUPRD2,0            FROM 'TO' PACKAGE (IF PRESENT)               
         DROP  RE                                                               
*                                                                               
*                                                                               
BD140    MVC   NBSTSTAT,FRSTTYPE   FROM STATION TYPE                            
         MVC   NBSTPSTT,FRPTTYPE   FROM POSTING TYPE                            
         MVC   NBSUBMED,FRSUBMED   SUB MEDIA CODE                               
         GOTO1 VEDIT,DMCB,(C'I',UNBLOCK)                                        
*                                                                               
         CLI   LUPVPHSW,YES                                                     
         BNE   BD200                                                            
         MVI   UNACTSW,C'A'                                                     
         MVI   UNLUPVPH,YES                                                     
         GOTO1 VEDIT,DMCB,(C'D',UNBLOCK)                                        
*                                                                               
BD200    GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'99',(R4)),0                        
         SPACE                                                                  
         GOTO1 VEDIT,DMCB,(C'F',UNBLOCK)                                        
         SPACE                                                                  
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,NBAIO                                   
         MVC   UNITDA,NDXDA        SAVE DISK ADDRESS                            
         GOTO1 VEDIT,DMCB,(C'P',UNBLOCK),AIOAREA4                               
         L     R3,AIOAREA4                                                      
*                                                                               
BD300    CLI   0(R3),X'FF'                                                      
         BE    BD400                                                            
         BAS   RE,NEWPTR                                                        
         LA    R3,25(R3)                                                        
         B     BD300                                                            
         SPACE                                                                  
BD400    MVC   ADDKA,NDXDA                                                      
         CLI   SUB,2                                                            
         BNE   *+8                                                              
         BAS   RE,SUBPRT                                                        
         L     R1,PRGUNITS                                                      
         LA    R1,1(R1)                                                         
         ST    R1,PRGUNITS                                                      
         B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
* ACTION DELETE                                                                 
*                                                                               
*--NOTE R8 IS USED IN TIS ROUTINE SO NO TWA REFERENCES CAN BE USED              
DELETE   NTR1                                                                   
         LA    R8,DELKEY           LIST OF KEYS TO BE DELETED                   
         CLI   0(R8),X'FF'                                                      
         BE    DELEX                                                            
*                                                                               
         L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
*                                                                               
DEL10    XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R8)                                               
         GOTO1 AIO,DMCB,UPDATE+UNT+DIR+READ                                     
         GOTO1 (RF),(R1),UPDATE+UNT+FILE+GET,NBAIO                              
*                                                                               
         LA    R3,BLOCK                                                         
         USING UNBLOCKD,R3                                                      
         XC    UNBLOCK,UNBLOCK     BUILD EDIT BLOCK                             
         ST    R9,UNAGLOB                                                       
         LA    RE,TEMPD+1500                                                    
         ST    RE,UNALOCAL                                                      
         ST    R4,UNAREC                                                        
         MVC   UNODATE,NUKDATE     SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NUKSUB                                                    
         MVC   UNOTIME,NUKTIME                                                  
         MVC   UNODAY,NUDAY                                                     
         GOTO1 VEDIT,DMCB,(C'I',(R3))                                           
         SPACE 1                                                                
         OI    KEY+NDIRCTL,X'80'   TURN ON DELETE BIT                           
         OI    NURSTAT,X'80'       PUT RECORD BACK AS DELETE TO RECV            
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
         GOTO1 (RF),(R1),UNT+FILE+PUT,NBAIO                                     
         GOTO1 VEDIT,DMCB,(C'P',(R3)),(C'O',AIOAREA2)                           
         L     R2,AIOAREA2                                                      
         SPACE 1                                                                
DEL18    CLI   0(R2),X'FF'                                                      
         BE    DEL30                                                            
         BAS   RE,OLDPTR           DELETE OLD POINTER                           
         LA    R2,NDIRLEN(R2)                                                   
         B     DEL18                                                            
*                                                                               
DEL30    LA    R8,20(R8)           FORCE READ HIGH TO RESET SEQUENCE            
         CLI   0(R8),X'FF'                                                      
         BNE   DEL10                                                            
*                                                                               
DELEX    B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE                                                                  
* SUB-ROUTINE TO DELETE OLD PASSIVE POINTERS (AT ENTRY, R2 ADDRESSES            
* POINTERS)                                                                     
*                                                                               
OLDPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R2)  DELETE OLD POINTER                           
         GOTO1 AIO,DMCB,UPDATE+UNT+DIR+HIGH                                     
         CLC   KEY(L'NUKEY),KEYSAVE                                             
         BNE   OLDPTRX                                                          
         OI    KEY+NDIRCTL,X'80'   DELETE BIT                                   
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
OLDPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT SUB-LINE NUMBER                                       
* PPROG = PROGRAM TO BE ADDED                                                   
*                                                                               
GETSUB   NTR1                                                                   
         L     R1,NBAIO            POINT TO RECORD KEY                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   NUKPTYPE,X'84'      USE PASSIVE KEY TO FIND NEXT NUMBER          
         MVC   NUKPAM,NUKAM-NUKEY(R1)                                           
         MVC   NUKPCLT,NUKCLT-NUKEY(R1)                                         
         MVC   NUKPNET,NUKNET-NUKEY(R1)                                         
         MVC   NUKPPROG,PPROG                                                   
         MVC   NUKPDATE,NUKDATE-NUKEY(R1)                                       
         MVC   NUKPEST,NUKEST-NUKEY(R1)                                         
         LA    R0,UPDATE+PASSDEL+UNT+DIR+HIGH                                   
         SPACE                                                                  
GETSUB2  GOTO1 AIO,DMCB,(R0)                                                    
         CLC   KEY(NUKPSUB-NUKPKEY),KEYSAVE                                     
         BNE   GETSUB4                                                          
         LA    R0,UPDATE+PASSDEL+UNT+DIR+SEQ                                    
         CLI   NUKPSUB,SUBMAX                                                   
         BL    GETSUB2                                                          
         MVI   FERN,SUBERR         SUB-LINE LIMIT BLOWN                         
         MVC   XTRA(10),=C'COPY ENDED'                                          
         BAS   RE,CLRSAVE          STOP THE COPY AND                            
         MVI   MODE,0              FORCE RE-START                               
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE                                                                  
GETSUB4  LA    R4,KEYSAVE                                                       
         ZIC   R1,NUKPSUB                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SUB                                                           
         L     R4,NBAIO            POINT BACK TO RECORD                         
         USING NURECD,R4                                                        
         STC   R1,NUKSUB                                                        
         B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR NEW POINTERS (AT ENTRY, R3 ADDRESSES POINTER)                 
*                                                                               
NEWPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R3)  READ FOR NEW POINTER                         
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE TEST IF KEY FOUND                           
         BE    NEWPTR2                                                          
         MVC   KEY(L'NUKEY+1),0(R3)                                             
         MVC   KEY+21(4),UNITDA                                                 
         GOTO1 (RF),(R1),UNT+DIR+ADD                                            
         B     NEWPTRX                                                          
         SPACE                                                                  
NEWPTR2  MVI   KEY+20,0                                                         
         MVC   KEY+21(4),UNITDA                                                 
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
NEWPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE FIRST UNIT FOR DATE WHEN SECOND UNIT IS ADDED           
*                                                                               
SUBPRT   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,AGYMED                                                    
         MVC   NUKPCLT,CLIPK                                                    
         MVC   NUKPNET,NET                                                      
         L     R1,AIOAREA1                                                      
         MVC   NUKPPROG,PPROG                                                   
         MVC   NUKPDATE,NUKDATE-NUKEY(R1)                                       
         MVC   NUKPEST,NUKEST-NUKEY(R1)                                         
         MVI   NUKPSUB,1            SUB-LINE 1                                  
         MVC   NUKPDP,NUKDP-NUKEY(R1) EXTRACT DAYPART                           
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY+1),KEYSAVE                                    
         BNE   SUBPRTX             COULD NOT FIND IT-DELETED                    
         SPACE                                                                  
SUBPRT2  L     R4,AIOAREA4         GET THE RECORD                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,(R4)                                
         USING NURECD,R4                                                        
         MVI   NUSUBPRT,1                                                       
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         SPACE                                                                  
SUBPRTX  B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR TWA SAVE AREA                                            
*                                                                               
CLRSAVE  ST    RE,SAVEREG                                                       
         LA    RE,SVDATA                                                        
         LA    RF,SVDATAL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO VALIDATE PRODUCT CODE (AT ENTRY, NBAIO POINTS                  
* TO FROM CLIENT RECORD)                                                        
*                                                                               
VALPRD   ST    RE,SAVEREG                                                       
         LA    R0,220                                                           
         L     RE,NBAIO                                                         
         LA    RE,CLIST-CLTHDRD(RE) RE=A(PRODUCT LIST)                          
         MVI   FERN,PRDERR                                                      
*                                                                               
VALPRD2  OC    0(4,RE),0(RE)       TEST FOR EOL                                 
         BZ    VALPRD25            YES                                          
         CLC   FRPRD,0(RE)         TEST CODE VS. TABLE                          
         BE    VALPRD4                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,VALPRD2                                                       
*                                                                               
VALPRD25 LA    R0,35                                                            
         L     RE,NBAIO                                                         
         LA    RE,CLIST2-CLTHDRD(RE) RE=A(EXTENDED PRODUCT LIST)                
*                                                                               
VALPRD3  OC    0(4,RE),0(RE)       TEST FOR EOL                                 
         BZ    ERROR               YES                                          
         CLC   FRPRD,0(RE)         TEST CODE VS. TABLE                          
         BE    VALPRD4                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,VALPRD3                                                       
         B     ERROR                                                            
*                                                                               
VALPRD4  MVC   FRPRN,3(RE)         EXTRACT PRODUCT NUMBER                       
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHECK AN ESTIMATE OPEN FOR EACH PRODUCT                        
*                                                                               
CHKPRDES NTR1                                                                   
         CLC   FREST,ESTIMATE      TO/FROM ESTIMATES = OK                       
         BE    EXXMOD                                                           
         CLI   COPYSW,NO           TEST IF COPYING                              
         BE    EXXMOD                                                           
         MVC   XTRA,SPACES                                                      
         MVI   FERN,0              CHECK IF ERROR                               
         MVI   FSTREAD,YES         SET FOR 1ST READ                             
*                                                                               
PE100    GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    PE300               YES                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   PE100                                                            
         CLI   FSTREAD,YES         1ST READ                                     
         BE    *+12                                                             
         TM    NBSUBMSK,NBSBMPRG   TEST FOR CHANGE IN PROGRAM                   
         BO    PE300               YES                                          
         MVI   FSTREAD,NO          NO LONGER 1ST READ                           
         L     R2,NBAIO            POINT TO RECORD                              
         SPACE                                                                  
PE120    SR    R0,R0                                                            
         LR    R4,R2                                                            
         LA    R2,NUMAINEL-NUKEY(R2)  FIRST ELEMENT                             
         SPACE                                                                  
PE140    CLI   0(R2),0             TEST FOR EOR                                 
         BE    PE200                                                            
         CLI   0(R2),X'06'         TEST FOR MISSING/MAKE-GOOD/BILL/PAY          
         BE    *+12                                                             
         CLI   0(R2),X'07'                                                      
         BNE   PE160                                                            
         CLI   LUPVPHSW,YES                                                     
         BNE   PE160                                                            
         CLC   OLDPROG,NUMGPCOD-NUMGD(R2)                                       
         BNE   PE100               IF CHANGE PROG OPTION PROG MUST BE =         
PE160    CLI   0(R2),X'10'                                                      
         BE    PE100                                                            
         CLI   0(R2),X'12'                                                      
         BE    PE100                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PE140                                                            
         SPACE                                                                  
PE200    SR    RF,RF                                                            
         ICM   RF,1,NUPRD-NURECD(R4)                                            
         BZ    PE100               EXCEPT UNALLC                                
         LA    RE,PRDLST-1         PRDS TO CHECK                                
         AR    RE,RF                                                            
         STC   RF,0(RE)                                                         
         ICM   RF,1,NUPRD2-NURECD(R4)                                           
         BZ    PE100                                                            
         LA    RE,PRDLST-1         PRDS TO CHECK                                
         AR    RE,RF                                                            
         STC   RF,0(RE)                                                         
         B     PE100                                                            
         SPACE                                                                  
PE300    LA    R2,PRDLST           PRDS TO CHECK                                
         LA    R3,SVPRDLST         PRDS ALREADY CHECKED                         
         LA    R4,220              # OF POSSIBLE PRDS                           
*                                                                               
PE320    OC    0(1,R2),0(R2)                                                    
         BZ    PE420                                                            
         CLC   0(1,R3),0(R2)       LOOK UP PRD EST?                             
         BE    PE400                                                            
*                                                                               
         BAS   RE,SETPRDCD                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM    AGY/MED/CLI                                  
         MVC   KEY+4(3),NPRDCDE    PRD CODE                                     
         MVC   KEY+7(1),ESTIMATE                                                
         ZIC   R0,NBDMGOPT                                                      
         ST    R3,WORK                                                          
         L     R3,AIOAREA2                                                      
         GOTO1 NBDM,DMCB,((R0),=CL8'DMREAD'),=CL8'SPTDIR',KEY,(R3),0            
         L     R3,WORK                                                          
         CLI   8(R1),0                                                          
         BE    PE400                                                            
         MVI   FERN,NOESTPRD                                                    
         LA    R0,23                                                            
         LA    RE,XTRA+23          END OF LIST                                  
         CLI   0(RE),C' '                                                       
         BNE   *+14                                                             
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
         B     *+8                                                              
         LA    RE,2(RE)                                                         
*                                                                               
         MVC   0(3,RE),NPRDCDE                                                  
         LA    RF,COPYESH                                                       
         ST    RF,FADDR                                                         
         LA    RF,17                                                            
         CR    R0,RF                                                            
         BH    PE500                                                            
         B     *+10                                                             
*                                                                               
PE400    MVC   0(1,R3),0(R2)       LOOK UP PRD EST?                             
PE420    LA    R2,1(R2)            NEXT PRD TO LOOK UP                          
         LA    R3,1(R3)            NEXT PRD THAT HAS BEEN LOOKED UP             
         BCT   R4,PE320                                                         
*                                                                               
PE500    LR    RE,R6                                                            
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,SVNBLOCK                                                      
         MVCL  RE,R0                                                            
         MVC   FRPRN,SVFRPRN       RESTORE SAVED PRODUCT NUMBER                 
         MVC   NBAIO,AIOAREA1      REFRESH BLOCK ADDRESSES                      
         MVC   NBACOM,ACOMFACS                                                  
         MVI   NBFUNCT,NBFRDHI     FORCE READ HIGH TO RESET SEQUENCE            
         MVC   NBSELEST,FREST                                                   
         CLI   FERN,0                                                           
         BNE   ERROR                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE PRODUCT CODE (AT ENTRY, NBAIO POINTS                  
* TO FROM CLIENT RECORD)                                                        
*                                                                               
SETPRDCD ST    RE,SAVEREG                                                       
         LA    R0,255                                                           
         LA    RE,CLILIST          RE=A(PRODUCT LIST)                           
*                                                                               
PC100    OC    0(4,RE),0(RE)       TEST FOR EOL                                 
         BZ    PCERR               YES                                          
         CLC   0(1,R2),3(RE)       TEST NUM VS. TABLE                           
         BE    PC200                                                            
         LA    RE,4(RE)                                                         
         BCT   R0,PC100                                                         
PCERR    DC    H'0'                                                             
*                                                                               
PC200    MVC   NPRDCDE,0(RE)       EXTRACT PRODUCT NAME                         
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* CHECK THE REASON CODE AGAINST THE "TO PACKAGE"                                
*        R2 = RECORD TO READ                                                    
*                                                                               
CHKREASN NTR1                                                                   
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA AT END OF ACTION            
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   CKRSNX              NO, REASON CODE NOT INPUTTED                 
*                                                                               
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    CKRSNX              NO, REASON CODE NOT INPUTTED                 
*                                                                               
         MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ERROR                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22                                                 
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
*                                                                               
*CKRSN20 MVI   FERN,AUDITERR                                                    
*        L     RE,NBAIO                                                         
*        USING NPRECD,RE                                                        
*        TM    NPAKSTAT,X'02'      IS REASON CODE REQUIRED                      
*        BO    CKRSN30             YES                                          
*        OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
*        BZ    CKRSNX                                                           
*        B     ERROR                                                            
*CKRSN30 OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
*        BZ    ERROR                                                            
*                                                                               
CKRSNX   B     EXXMOD                                                           
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
EOFMSG   DC    C'** COPY COMPLETED - ENTER NEXT ACTION **'                      
NOUNITS  DC    C'** ERROR - NO UNITS IN FROM PACKAGE **'                        
MDGERR   DC    C'** ERROR - MADE-GOOD UNIT CANNOT BE TRANSFERRED'               
MKGERR   DC    C'** ERROR - MAKE-GOOD UNIT CANNOT BE TRANSFERRED'               
BILLERR  DC    C'** ERROR - BILLED UNIT CANNOT BE TRANSFERRED'                  
PAIDERR  DC    C'** ERROR - PAID UNIT CANNOT BE TRANSFERRED'                    
UNTFILE  DC    CL8'UNTFILE'                                                     
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* COPY SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF2D                                                       
* SAVE AREA                                                                     
         ORG   TWAD+PAGELEN                                                     
*                                                                               
SVDATA   DS    0D                                                               
SVTOPACK DS    X                                                                
SVFRCLT  DS    XL2                                                              
SVFREST  DS    X                                                                
SVFRPACK DS    X                                                                
SVFRPRD  DS    CL3                                                              
SVFRPRN  DS    X                                                                
SVPROG   DS    CL6                                                              
SVPGDYTM DS    CL5                 SAVE PROGRAM DAY/TIME                        
SVUNITS  DS    F                                                                
SVPRDLST DS    CL220               PRDS THAT HAVE THIS EST                      
         DS    0D                                                               
SVNBLOCK DS    XL(NEBLOCKL)                                                     
SVDATAL  EQU   *-SVDATA                                                         
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
UNITDA   DS    XL4                 SAVED DISK ADDRESS                           
*                                                                               
PRGUNITS DS    F                                                                
FRCLT    DS    XL2                 PACKED FROM CLIENT CODE                      
FREST    DS    X                   FROM ESTIMATE                                
FRPACK   DS    X                                                                
FRPRD    DS    CL3                 FROM PRODUCT CODE                            
FRPRN    DS    X                   FROM PRODUCT NUMBER                          
FRSTTYPE DS    X                   FROM STATION TYPE                            
FRPTTYPE DS    X                   FROM POSTING TYPE                            
FRSUBMED DS    X                   FROM SUB MEDIA                               
TOPACK   DS    X                                                                
COPYSW   DS    C                                                                
FRSTSW   DS    C                                                                
SUB      DS    X                                                                
ALPRD    DS    CL1                 SAVE ALLOCATED PRDS                          
LUPVPHSW DS    CL1                 LOOK UP NEW DEMO'S                           
NPRDCDE  DS    CL3                 NEW PRD TO READ EST FOR                      
FSTREAD  DS    CL1                 1ST READ DON'T DO PRG CHECK                  
OLDPROG  DS    CL6                 IF CHANGING PROGRAM OLD PROGRAM              
PPROG    DS    CL6                 PROGRAM PARIMETER                            
PRDLST   DS    CL220               LIST OF PRODUCTS TO CHECK ESTS               
UNITLST  DS    CL46                DATE LINE NUMBER TABLE                       
DELKEY   DS    CL301               SAVE AREA FOR KEYS TO DELETE                 
ADDKA    DS    F                   LAST DISK ADDRESS ADDED                      
MKGTBLL  EQU   14                  LENGTH OF ENTIES INTO TABLE                  
MKGTBLN  EQU   40                  NUMBER OF ENTRIES ALLOWED IN TABLE           
*                                                                               
         DS    0D                                                               
BLOCK    DS    CL256                                                            
MKGSV    DS    CL560                                                            
         DS    XL1                                                              
MKGRDTBL DS    CL360                                                            
         DS    XL1                                                              
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
SLASH    EQU   C'/'                                                             
         SPACE 2                                                                
* SPGENCLT (CLTHDRD)                                                            
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENEST (ESTHDRD)                                                            
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NETBILLRD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011NEBUY27   12/13/10'                                      
         END                                                                    
