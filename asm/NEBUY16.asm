*          DATA SET NEBUY16    AT LEVEL 022 AS OF 12/13/10                      
*PHASE T31116A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - COPY UNITS OVERLAY - T31116'               
T31116   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**COPY**,RA,RR=RE                                              
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
*        TM    FLDH+4,X'04'        TEST ALPHA DATA                              
*        BZ    ERROR                                                            
         MVC   FRPRD,FLD                                                        
*                                                                               
IC420    CLC   FRPRD,SVFRPRD       TEST FOR CHANGE IN FROM PRODUCT              
         BE    *+8                                                              
         OI    MODE,FIRST                                                       
         MVC   SVFRPRD,FRPRD                                                    
         SPACE                                                                  
* EDIT PRODUCT ALLOCATION                                                       
*                                                                               
IC500    LA    R2,COPFRPAH                                                      
         XC    ALPRD,ALPRD                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    IC510               NO INPUT                                     
         MVI   FERN,INVERR                                                      
         CLI   FLD,NO              VALIDATE NO                                  
         BE    IC510                                                            
         CLI   FLD,YES             VALIDATE YES                                 
         BNE   ERROR                                                            
         CLC   BUYCLI,COPFCLT                                                   
         BNE   ERROR                                                            
         MVC   ALPRD,FLD                                                        
         SPACE                                                                  
* EDIT TRTAFFIC TRANSFER                                                        
*                                                                               
IC510    LA    R2,COPFRTRH                                                      
         MVI   ALTRAF,C'N'         DEFAULT IS NO                                
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    IC520               NO INPUT                                     
         CLI   FLD,NO              VALIDATE NO                                  
         BE    IC520                                                            
*                                                                               
         MVI   FERN,INVERR                                                      
         CLI   FLD,YES             VALIDATE YES                                 
         BNE   ERROR                                                            
*                                                                               
         CLC   FRCLT,SVFRCLT       TRAFFIC SHOULD NOT BE COPIED                 
         BNE   ERROR               IF FROM AND TO CLIENTS ARE DIFF.             
         CLI   COPFRPA,NO          OR IF COPY BRANDS = NO                       
         BE    ERROR                                                            
         MVI   ALTRAF,C'Y'                                                      
         SPACE                                                                  
                                                                                
* EDIT LENGTH FILTER                                                            
*                                                                               
IC520    LA    R2,COPFRLNH                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    IC540               NO INPUT                                     
*                                                                               
         ZIC   R1,FLDH+5           SAVE DATA LENGTH                             
         LA    RE,FLD              RE=DATA POINTER                              
IC525    CLI   0(RE),C'0'          TEST FOR NUMERIC DATA                        
         BL    ERROR                                                            
         CLI   0(RE),C'9'                                                       
         BH    ERROR                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,IC525                                                         
*                                                                               
         ZIC   R1,FLDH+5           SAVE DATA LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   RE,DUB                                                           
*                                                                               
         LTR   RE,RE               TEST FOR ZERO                                
         BZ    ERROR                                                            
         CH    RE,=H'255'          TEST FOR MAXIMUM VALUE                       
         BH    ERROR                                                            
         STCM  RE,1,FRLEN                                                       
         SPACE 1                                                                
* EDIT TO PACKAGE                                                               
*                                                                               
IC540    LA    R2,COPTOPKH                                                      
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
* EDIT START AND END DATE                                                       
*                                                                               
         XC    DATES,DATES         CLEAR START AND END DATE                     
         MVI   FERN,DATERR         SET ERROR MESSAGE                            
         LA    R2,COPFSTRH         START DATE INPUT HEADER                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            ANY START DATE INPUT?                        
         BE    IC690                                                            
         CLI   FLDH+5,5            SHORT HAND INPUT?                            
         BH    IC680                                                            
         MVC   TEMPDATE,COPFSTR                                                 
         BAS   RE,DATEFOR          FILL SHORT HAND DATA WITH DEFAULT            
         MVC   COPFSTR,TEMPDATE    YEAR                                         
IC680    GOTO1 VDATVAL,DMCB,(0,COPFSTR),WORKDATE                                
         CLC   =F'0',DMCB                                                       
         BE    ERROR               VALIDATE INPUT                               
         GOTO1 VDATCON,DMCB,(0,WORKDATE),(2,STADATE)                            
         GOTO1 VDATCON,DMCB,(0,WORKDATE),(5,COPFSTR)                            
         OI    COPFSTRH+6,X'80'    REFRESH SCREEN                               
*                                                                               
IC690    LA    R2,COPFENDH         END DATE INPUT HEADER                        
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            ANY DATA?                                    
         BE    IC696                                                            
         CLI   FLDH+5,5            SHROT HAND?                                  
         BH    IC694                                                            
         MVC   TEMPDATE,COPFEND    FILL SHORT HAND DATA WITH DEFAULT            
         BAS   RE,DATEFOR          YEAR                                         
         MVC   COPFEND,TEMPDATE                                                 
IC694    GOTO1 VDATVAL,DMCB,(0,COPFEND),WORKDATE                                
         CLC   =F'0',DMCB          VALIDATE DATE INPUT                          
         BE    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORKDATE),(2,ENDATE)                             
         GOTO1 VDATCON,DMCB,(0,WORKDATE),(5,COPFEND)                            
         OI    COPFENDH+6,X'80'    REFRESH SCREEN                               
*                                                                               
IC695    MVI   FERN,SEQERR         DATE ORDER ERROR                             
         LA    R2,COPFSTRH         RESET INPUT FEILD                            
         GOTO1 VGETFLD                                                          
         CLC   ENDATE,STADATE      CHECK FOR DATE SEQENCE                       
         BNL   IC696               ERROR                                        
         B     ERROR                                                            
*                                                                               
IC696    CLC   DATES,SSTADATE      CHECK FOR FIRST TIME ENTRY                   
         BE    *+8                                                              
         OI    MODE,FIRST                                                       
         MVC   SSTADATE,DATES                                                   
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
*        TM    NPAKSTAT,X'02'      IS PACKAGE AUDIT SET                         
*        BO    ERROR               YES, CANT DO COPY                            
         MVI   FERN,PAKLERR                                                     
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BO    ERROR                                                            
         MVI   FERN,PAKFERR        TEST FOR FROZEN PACKAGE                      
         TM    NPAKSTAT,X'80'                                                   
         BO    ERROR                                                            
         BAS   RE,CHKREASN         CHECK REASON CODE                            
         MVC   TOPAKCTL,NPAKCNTL   SAVE PACKAGE CONTROL BYTE                    
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
         GOTO1 VBLDRQST            GENERATE TURN AROUND                         
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
         GOTO1 VCHKSEC,DMCB,NBAIO,COPFCLT                                       
*                                                                               
****     OC    TWAACCS(2),TWAACCS  TEST FOR SECURITY LIMITS                     
****     BZ    IC900                                                            
****     CLI   TWAACCS,C'+'        TEST FOR MARKET LOCKET                       
****     BE    IC900               NO                                           
****     MVI   FERN,SCTYERR                                                     
****     CLI   TWAACCS,C'$'        TEST FOR OFFICE LIST                         
****     BE    IC830                                                            
****     CLI   TWAACCS,C'*'        TEST FOR OFFICE LIMIT                        
****     BE    IC860                                                            
****     CLC   TWAACCS(2),FRCLT    TEST FOR FILTERED CLIENT                     
****     BE    IC900               OK                                           
****     B     ERROR                                                            
*                                                                               
****IC830    DS    0H               * TEST OFFICE LIST SECURITY *               
****     XC    DMCB(8),DMCB                                                     
****     MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
****     L     RF,VCALLOV                                                       
****     GOTO1 (RF),DMCB                                                        
****     CLI   4(R1),X'FF'                                                      
****     BNE   *+6                                                              
****     DC    H'0'                                                             
****     XC    DUB,DUB                                                          
****     LA    R1,DUB                                                           
****     USING OFFICED,R1                                                       
****     MVI   OFCSYS,C'S'         SYSTEM ID                                    
****     MVC   OFCAUTH,TWAACCS     ID AUTH VALUE                                
****     MVC   OFCAGY,AGENCY                                                    
****     MVC   OFCOFC,COFFICE                                                   
*                                                                               
****     L     RF,DMCB                                                          
****     GOTO1 (RF),DMCB,DUB,ACOMFACS,0                                         
****     CLI   0(R1),0                                                          
****     BNE   ERROR                                                            
****     B     IC900                                                            
*                                                                               
****IC860    CLC   TWAACCS+1(1),COFFICE TEST FOR FILTERED OFFICE                
****     BNE   ERROR                                                            
         SPACE                                                                  
* VALIDATE FROM PRODUCT                                                         
*                                                                               
IC900    MVI   SVFRPRN,0           INITIALIZE LAST PRODUCT NUMBER               
         OC    FRPRD,FRPRD         TEST FOR PRODUCT FILTER                      
         BZ    IC920                                                            
         LA    R2,COPFRPRH                                                      
         ST    R2,FADDR            SET CURSOR ERROR POSITION                    
         BAS   RE,VALPRD           VALIDATE CODE                                
         CLI   FERN,0           CHECK ERROR RETURN                              
         BNE   ERROR                                                            
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
         L     R4,NBAIO                                                         
         USING NPRECD,R4                                                        
         MVI   FERN,AUDACTER                                                    
*        TM    NPAKSTAT,X'02'      IS PACKAGE AUDIT SET                         
*        BO    ERROR               YES, CANT DO COPY                            
         MVI   FERN,PAKLERR                                                     
         TM    NPAKSTAT,X'20'                                                   
         BO    ERROR                                                            
         MVI   FERN,PAKFERR                                                     
         TM    NPAKSTAT,X'80'                                                   
         BO    ERROR                                                            
*                                                                               
         TM    NPAKCNTL,X'40'      IS FROM PKG IMP BASED                        
         BZ    IC1100              NO CHECK VPH BASE                            
         TM    TOPAKCTL,X'40'      IS TO PKG IMP BASED                          
         BZ    IC1200              NO ERROR                                     
         B     IC1300                                                           
IC1100   TM    TOPAKCTL,X'40'      IS TO PKG VPH BASED                          
         BZ    IC1300              YES OK                                       
*                                                                               
IC1200   MVC   BUYMSG(L'DEMOMTCH),DEMOMTCH                                      
         MVI   MODE,0                                                           
         BAS   RE,CLRSAVE          CLEAR SAVE AREA                              
         LA    R2,COPFRPKH                                                      
         ST    R2,FADDR                                                         
         GOTO1 VEXIT                                                            
*                                                                               
IC1300   XC    COPFPKD,COPFPKD     OUTPUT THE PACKAGE DESCRIPTION               
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
         XCEF  SVPRDLST,2000                                                    
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
*                                                                               
         LA    R2,COPNPRGH         EDIT NEW PROGRAM FIELD                       
         GOTO1 VGETFLD                                                          
         MVC   OLDPROG,SVPROG      SAVE OLD PROGRAM                             
         CLI   FLDH+5,0                                                         
         BE    CP320                                                            
         MVC   XTRA(13),=C'DUPLICATE PROGRAM'                                   
         MVI   FERN,INVERR                                                      
         MVC   PROG,FLD                                                         
         CLC   PROG,SVPROG                                                      
         BE    ERROR                                                            
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         CLI   FERN,0                                                           
         BNE   ERROR                                                            
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         MVC   XTRA(19),=C'DAY/TIME DIFFERANCE'                                 
         CLC   SVPGDYTM,NPGDAY     DAY/TIME MUST BE EQUAL                       
         BNE   ERROR                                                            
         MVC   SVPROG,PROG                                                      
         MVI   LUPVPHSW,YES                                                     
         XC    XTRA,XTRA                                                        
         DROP  RE                                                               
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
         BE    EC100               YES                                          
*                                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BNE   CP400                                                            
         CLI   FRSTSW,YES          TEST FOR RE-READ OF FIRST RECORD             
         BE    CP500               YES-SKIP PROGRAM BREAK CHECK                 
         TM    NBSUBMSK,NBSBMPRG   TEST FOR CHANGE IN PROGRAM                   
         BO    EC100               YES                                          
         CLC   CNTUNITS,=F'50'     TEST IF 50 RECORDS TRANSFERED                
         BL    CP500               YES                                          
         MVI   FULLREAD,C'Y'       SET SWITCH                                   
         B     EC100                                                            
*                                                                               
         SPACE                                                                  
CP500    MVI   FRSTSW,NO                                                        
         CLI   COPYSW,NO           TEST IF COPYING                              
         BE    CP400               NO-READ UNTIL EOF OR CONTROL BREAK           
         OC    FRLEN,FRLEN         TEST FOR LENGTH FILTER                       
         BZ    *+14                NO                                           
         CLC   FRLEN,NBLEN         FILTER ON TOTAL LENGTH                       
         BNE   CP400               NO-NEXT RECORD                               
*                                                                               
         OC    FRPRD,FRPRD         TEST FOR PRODUCT FILTER                      
         BZ    CP550               NO                                           
         L     R2,NBAIO                                                         
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',(R2)),0                        
         CLI   12(R1),0                                                         
         BNE   CP530                                                            
         L     R3,12(R1)                                                        
         USING NUPDED,R3                                                        
         CLC   NUPDEPR,FRPRD        CHECK ALPHA PRODUCT                         
         BNE   CP400                                                            
         B     CP550                                                            
         DROP  R3                                                               
*                                                                               
CP530    CLI   NBPRD,0             IF UNIT UNNALLOCATED                         
         BE    CP400               BYPASS                                       
         CLC   FRPRN,NBPRD         FILTER ON FIRST PRODUCT                      
         BNE   CP400               NO-NEXT RECORD                               
*                                                                               
CP550    CLC   NBACTDAT,STADATE    FILTER ON FIRST DATE                         
         BL    CP400               NO-NEXT RECORD                               
         OC    ENDATE,ENDATE       FILTER ON END DATE?                          
         BZ    NOFILTER            NO FILTER, CONTINUE                          
         CLC   NBACTDAT,ENDATE     TEST FOR END DATE FILTER                     
         BH    CP400               NO-NEXT RECORD                               
*                                                                               
NOFILTER SR    R0,R0                                                            
         SR    R3,R3                                                            
         L     R2,NBAIO            POINT TO RECORD                              
         LA    R2,NUMAINEL-NUKEY(R2)  FIRST ELEMENT                             
         SPACE                                                                  
CP600    CLI   0(R2),0             TEST FOR EOR                                 
         BE    CP650                                                            
         CLI   0(R2),X'07'         TEST FOR MISSING/MAKE-GOOD/BILL/PAY          
         BE    CP400                                                            
         CLI   0(R2),X'10'                                                      
         BNE   CP620                                                            
         USING NUBILD,R2                                                        
         TM    NUBILST,X'20'       UNBILLED ELEMENTS CAN GO THRU                
         BZ    CP400                                                            
         DROP  R2                                                               
*                                                                               
CP620    CLI   0(R2),X'12'                                                      
         BE    CP400                                                            
         CLI   0(R2),X'06'         IF LAST MKG ADD MISSED + MKG UNITS           
         BNE   *+6                                                              
         LR    R3,R2               SAVE IF MAKEGOOD ONLY                        
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     CP600                                                            
*                                                                               
CP650    DS    0H                                                               
         XC    DMWORK(96),DMWORK                                                
         LA    RE,DMWORK                                                        
         STCM  RE,15,NBABILRD       ADDRESS OF BILL DSECT IN NETBLOCK           
         USING NBLBILLD,RE                                                      
         MVC   NBLUNAIO,NBAIO                                                   
         OI    NBLFUNC,NBLBLD                                                   
         GOTO1 VBILLRDR,DMCB,NETBLOCK                                           
         LA    RE,DMWORK                                                        
         TM    NBLFUNC,NBLBILD                                                  
         BO    CP400                                                            
*                                                                               
CP700    LTR   R2,R3                R3 CONT MAKEGOOD ELEM ADDRESS               
         BNZ   CP800                                                            
         L     R1,NBAIO                                                         
         USING NURECD,R1                                                        
         TM    NUUNITST,X'40'      TEST FOR PRE-EMPT                            
         BNZ   CP400                                                            
         DROP  R1                                                               
         SPACE                                                                  
         MVC   PPROG,SVPROG        PROGRAM PERIMATER                            
         BAS   RE,BUILD            CONSTRUCT RECORD/WRITE TO FILE               
         B     CP400               GET NEXT RECORD                              
         SPACE                                                                  
CP800    BAS   RE,ADDMKGS                                                       
         B     CP400                                                            
         EJECT                                                                  
* EOF/NEW PROGRAM PROCESSING                                                    
*                                                                               
EC100    XC    COPPRDS,COPPRDS     CLEAR PROGRAM DESCRIPTION                    
         OI    COPPRDSH+6,X'80'    XMIT                                         
         XC    COPSUM,COPSUM       CLEAR SUMMARY LINE                           
         OI    COPSUMH+6,X'80'                                                  
         L     R3,SVPGUNTS                                                      
         TM    MODE,FIRST                                                       
         BO    EC200                                                            
         TM    NBSUBMSK,NBSBMPRG   TEST FOR CHANGE IN PROGRAM                   
         BO    *+12                YES                                          
         CLI   FULLREAD,C'Y'                                                    
         BE    EC250                                                            
*                                                                               
         LA    R2,COPSUM                                                        
         EDIT  (R3),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                            
         AR    R2,R0               UPDATE OUTPUT POINTER                        
         MVC   1(16,R2),=C'UNITS COPIED FOR'                                    
         LA    R2,18(R2)                                                        
         MVC   0(L'SVPROG,R2),SVPROG    LAST PROGRAM                            
         SPACE                                                                  
EC200    A     R3,SVUNITS          UPDATE COUNT OF COPIED UNITS                 
         ST    R3,SVUNITS                                                       
EC250    CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    EC600                                                            
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
EC300    CLI   FULLREAD,C'Y'                                                    
         BE    EC400                                                            
         LA    R2,COPYESH                                                       
*        NI    COPYESH+1,X'DF'     MAKE FIELD UN-PROTECTED                      
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
         XC    SVPGUNTS,SVPGUNTS                                                
         XC    CNTUNITS,CNTUNITS                                                
         MVI   FULLREAD,C'N'                                                    
         B     COPYX                                                            
         SPACE                                                                  
EC400    LA    R2,COPYESH                                                       
*        OI    COPYESH+1,X'20'     MAKE FIELD PROTECTED                         
         ST    R2,FADDR                                                         
         MVC   8(3,R2),=C'YES'     XMIT 'YES' IN COPY                           
         OI    6(R2),X'81'         MODIFIED NEXT TIME                           
         MVC   BUYMSG(39),=C'50 RECORDS COPIED HIT ENTER TO CONTINUE'           
         XC    CNTUNITS,CNTUNITS                                                
         MVI   FULLREAD,C'N'                                                    
         B     COPYX                                                            
         SPACE                                                                  
EC600    EDIT  (R3),(4,COPSUM),ALIGN=LEFT                                       
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
         L     R2,APACKREC                                                      
         USING NPRECD,R2                                                        
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
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'19',(R4)),0                        
* GET ALPHA PRODUCT CODE                                                        
         GOTO1 VHELLO,DMCB2,(C'G',UNTFILE),(X'02',(R2)),0                       
         CLI   12(R1),0            SET CC ON EXIT                               
         BNE   BD130                                                            
         L     RE,12(R1)                                                        
         USING NPK2D,RE                                                         
         CLI   NPAKMPRD,X'40'                                                   
         BNH   BD130                                                            
         XC    ELEM,ELEM                                                        
         LA    RF,ELEM                                                          
         USING NUPDED,RF                                                        
         MVI   NUPDEEL,X'19'                                                    
         MVI   NUPDELEN,10                                                      
         MVC   NUPDEPR,NPAKMPRD                                                 
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'19',(R4)),ELEM,0                   
         DROP  R2,RE,RF                                                         
*  REMOVE COPYSPLIT INFO IF ON UNIT                                             
*                                                                               
BD130    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   BD140                                                            
         L     RF,12(R1)                                                        
         USING NUSDRD,RF                                                        
         NI    NUSDST3,X'FF'-X'40' CLEAR COPYSPLIT INDICATOR                    
         DROP  RF                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'14',(R4)),0                        
*  REMOVE FLIGHT INFO IF ON UNIT                                                
*                                                                               
BD140    GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'60',(R4)),(1,=C'1')                
*                                                                               
         CLI   ALTRAF,C'Y'         COPY TRAFFIC TO NEW UNITS                    
         BE    BD160                                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'23',(R4)),0                        
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'21',(R4)),0                        
         XC    ELEM(80),ELEM                                                    
         MVC   ELEM(2),=XL2'2150'                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'21',(R4)),ELEM,0                   
*                                                                               
BD160    MVC   NBSTSTAT,FRSTTYPE   FROM STATION TYPE                            
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
         BAS   RE,SETAUDIT         SET THE AUDIT INFO IN THE BUY                
*                                                                               
         L     R4,NBAIO                                                         
         XC    NUAFFTIM,NUAFFTIM   CLEAR AFFID TIME                             
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   BD220                                                            
         L     RF,12(R1)                                                        
         USING NUSDRD,RF                                                        
         XC    NUSDAFDT,NUSDAFDT   CLEAR AFFID DATE                             
         NI    NUSDST3,X'FF'-X'04' CLEAR UNIT ADDED BY CABLE UPLOAD             
         DROP  RF                                                               
*                                                                               
BD220    DS    0H                                                               
         GOTO1 VHELLO,DMCB2,(C'G',UNTFILE),(X'02',(R2)),0                       
         CLI   12(R1),0            SET CC ON EXIT                               
         BNE   BD225                                                            
         L     RE,12(R1)                                                        
         USING NPK2D,RE                                                         
         MVC   PKGRCASH,NPK2CASH                                                
         MVC   PKGRTRAD,NPK2TRAD                                                
         DROP  RE                                                               
*                                                                               
BD225    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   BD230                                                            
         L     RF,12(R1)                                                        
         USING NUDTAD,RF                                                        
         XC    NUDTINVN,NUDTINVN   CLEAR TIME INVOICE #                         
         XC    NUDTIIVN,NUDTIIVN   CLEAR INTEGRATION INVOICE #                  
         MVC   NUDTCASH,PKGRCASH   MOVE IN BARTER PERCENT                       
         MVC   NUDTTRAD,PKGRTRAD   MOVE IN TRADE PERCENT                        
         DROP  RF                                                               
*                                                                               
*  SET UP BARTER PERCENT ON NEW RECORD                                          
BD230    DS    0H                                                               
         GOTO1 VCALCASH,DMCB,NBAIO,ACOMFACS,(1,CLICPRD)                         
*                                                                               
BD250    DS    0H                                                               
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
         L     R1,SVPGUNTS                                                      
         LA    R1,1(R1)                                                         
         ST    R1,SVPGUNTS                                                      
*                                                                               
         L     R1,CNTUNITS                                                      
         LA    R1,1(R1)                                                         
         ST    R1,CNTUNITS                                                      
         B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
* MOVE TO PACKAGE AUDIT INFO INTO THE UNITS                                     
*                                                                               
SETAUDIT NTR1                                                                   
         L     R3,APACKREC                                                      
         USING NPRECD,R3                                                        
         L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
*                                                                               
         NI    NUPACKST,X'FD'        SET AUDIT TO OFF                           
*                                                                               
*  REMOVE AUDIT CODE AND COMMENT ELEMENT ON UNIT                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'09',(R4)),0                        
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R4)),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,12(R1)                                                        
         USING NUSDRD,R2                                                        
         NI    NUSDST4,X'DF'        TURN OFF NEW STATUS                         
*                                                                               
         TM    NPAKSTAT,X'02'       IS AUDIT ON IN PACKAGE                      
         BZ    SETAUDEX             NO EXIT                                     
*                                                                               
         OI    NUSDST4,X'20'        TURN ON NEW STATUS                          
         DROP  R2                                                               
*                                                                               
         OI    NUPACKST,X'02'       SET AUDIT STATUS TO ON                      
*                                                                               
*  MOVE AUDIT CODE AND COMMENT ELEMENT TO UNIT                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'09',PACKREC),0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,12(R1)                                                        
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(X'09',(R4)),(R2),0                   
*                                                                               
SETAUDEX B     EXXMOD                                                           
         DROP  R3,R4                                                            
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
VALPRD   NTR1                                                                   
* FIRST VALIDATE THE ALPHA CODE                                                 
         MVI   FERN,0                                                           
         XC    KEY,KEY             NOW VALIDATE BRAND ESTIMATE                  
         LA    R3,KEY                                                           
         USING PRDHDR,R3                                                        
         MVC   PLSTTYPE(2),=XL2'0DF1'                                           
         MVC   PLSTAM,AGYMED                                                    
         MVC   PLSTCLT,CLIPK                                                    
         MVC   PLSTPRD,FRPRD       PRODUCT CODE                                 
         CLC   FRPRD,=C'POL'       TEST FOR POOL                                
         BE    *+14                YES                                          
         CLC   THREE,=C'AAA'       TEST FOR AAA                                 
         BNE   *+8                 NO                                           
         B     VALPRDER                                                         
*                                                                               
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(9),KEYSAVE      TEST IF PRODUCT FOUND                        
         BNE   VALPRDER            NO NOT FOUND                                 
         MVC   FRPRN,PLSTBPRD+1    EXTRACT PRODUCT NUMBER                       
         B     VALPRDX                                                          
*                                                                               
VALPRDER MVI   FERN,PRDERR                                                      
VALPRDX  B     EXXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO VALIDATE PRODUCT CODE (AT ENTRY, NBAIO POINTS                  
* TO FROM CLIENT RECORD)                                                        
*                                                                               
***VALPRD   ST    RE,SAVEREG                                                    
***         LA    R0,220                                                        
***         L     RE,NBAIO                                                      
***         LA    RE,CLIST-CLTHDRD(RE) RE=A(PRODUCT LIST)                       
***         MVI   FERN,PRDERR                                                   
*                                                                               
***VALPRD2  OC    0(4,RE),0(RE)       TEST FOR EOL                              
***         BZ    VALPRD25            YES                                       
***         CLC   FRPRD,0(RE)         TEST CODE VS. TABLE                       
***         BE    VALPRD4                                                       
***         LA    RE,4(RE)                                                      
***         BCT   R0,VALPRD2                                                    
*                                                                               
***VALPRD25 LA    R0,35                                                         
***         L     RE,NBAIO                                                      
***         LA    RE,CLIST2-CLTHDRD(RE) RE=A(EXTENDED PRODUCT LIST)             
*                                                                               
***VALPRD3  OC    0(4,RE),0(RE)       TEST FOR EOL                              
***         BZ    ERROR               YES                                       
***         CLC   FRPRD,0(RE)         TEST CODE VS. TABLE                       
***         BE    VALPRD4                                                       
***         LA    RE,4(RE)                                                      
***         BCT   R0,VALPRD3                                                    
***         B     ERROR                                                         
*                                                                               
***VALPRD4  MVC   FRPRN,3(RE)         EXTRACT PRODUCT NUMBER                    
***         L     RE,SAVEREG                                                    
***         BR    RE                                                            
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
PE200    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'19',(R4)),0                        
         CLI   12(R1),0                                                         
         BNE   PE240                                                            
         L     R3,12(R1)                                                        
         USING NUPDED,R3                                                        
         ZIC   R1,NUPDELEN                                                      
         LA    R2,NUPDEPR                                                       
         S     R1,=F'3'                                                         
         SR    R0,R0                                                            
         D     R0,=F'7'                                                         
         LR    R3,R1                R3 = NUMBER OF PRODUCTS                     
PE220    MVC   NPRDCDE,0(R2)                                                    
         BAS   RE,LDPRDTAB                                                      
         LA    R2,7(R2)             NEXT PRODUCT                                
         BCT   R3,PE220                                                         
         B     PE100                                                            
         DROP  R3                                                               
*                                                                               
PE240    CLI   NUPRD-NURECD(R4),0                                               
         BE    PE100               EXCEPT UNALLC                                
         LA    R2,NUPRD-NURECD(R4)                                              
         BAS   RE,SETPRDCD                                                      
         BAS   RE,LDPRDTAB                                                      
*                                                                               
         CLI   NUPRD2-NURECD(R4),0                                              
         BE    PE100               EXCEPT UNALLC                                
         LA    R2,NUPRD2-NURECD(R4)                                             
         BAS   RE,SETPRDCD                                                      
         BAS   RE,LDPRDTAB                                                      
         B     PE100                                                            
         SPACE                                                                  
PE300    LA    R3,SVPRDLST         PRDS TO CHECK                                
         LA    R4,500              # OF POSSIBLE PRDS                           
*                                                                               
PE320    CLI   3(R3),X'FF'          CHECK IF ALREADY VALIDATED                  
         BE    PE400                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM    AGY/MED/CLI                                  
         MVC   KEY+4(3),0(R3)      PRD CODE                                     
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
         MVC   0(3,RE),0(R3)        NON MATCHING PRODUCT CODE                   
         LA    RF,COPYESH                                                       
         ST    RF,FADDR                                                         
         LA    RF,17                                                            
         CR    R0,RF                                                            
         BH    PE500                                                            
         B     PE420                                                            
*                                                                               
PE400    MVI   3(R3),X'FF'                                                      
PE420    LA    R3,4(R3)             BUMP TO NEXT PRODUCT                        
         OC    0(3,R3),0(R3)        CHECK END OF LIST                           
         BZ    PE500                                                            
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
* MOVE PRODUCT TO SVPRDLST TABLE                                                
* INPUT PRODUCT CODE NPRDCDE                                                    
*                                                                               
LDPRDTAB NTR1                                                                   
         LA    RE,SVPRDLST                                                      
         LA    R0,500                                                           
*                                                                               
LDPRD020 OC    0(4,RE),0(RE)       TEST FOR EOL                                 
         BZ    LDPRD100            YES                                          
         CLC   0(3,RE),NPRDCDE     TEST PRD VS. TABLE                           
         BE    LDPRDEX                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,LDPRD020                                                      
         DC    H'0'                                                             
*                                                                               
LDPRD100 MVC   0(3,RE),NPRDCDE                                                  
LDPRDEX  B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO ADD A CHAIN OF MAKEGOOD AND MISSED UNITS                       
*        R2 = MAKE GOOD ELEMENT ('06')                                          
*                                                                               
ADDMKGS  NTR1                                                                   
         LA    R3,MKGSV            CLEAR AREA STORE STORE MISSED UNITS          
         LR    RE,R3                                                            
         LA    RF,701              50 ENTRIES X 14 BYTES '0' TO END             
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,RDMDGUN1                                                      
         BNZ   EXXMOD              IF PAID/BILLED REJECT ALL                    
         SPACE                                                                  
* EXTRACT ALL RECORDS TO READ                                                   
MG100    L     R1,AIOAREA2                                                      
         SR    R0,R0               LOOK FOR ANOTHER MAKEGOOD                    
         LA    R1,NUMAINEL-NUKEY(R1)  FIRST ELEMENT                             
*                                                                               
MG120    CLI   0(R1),0             IF NO MKG THAN LAST MISSED IN CHAIN          
         BE    MG200                                                            
         CLI   0(R1),X'06'                                                      
         BE    *+12                A MAKEGOOD                                   
         CLI   0(R1),X'07'                                                      
         BNE   *+8                 A MISSED                                     
         BAS   RE,SVMSMK                                                        
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     MG120                                                            
         SPACE                                                                  
* READ ALL MISSED UNITS - CHECK FOR PAID OR BILLED - SAVE DKA/LN#               
MG200    BAS   RE,SELMKG           ** R2 - RETURNED WITH MKG TO READ            
         BZ    MG300                                                            
         BAS   RE,RDMDGUN                                                       
         BNZ   EXXMOD              IF PAID/BILLED REJECT ALL                    
         XC    0(9,R2),0(R2)       CLEAR OUT ENTRY JUST READ                    
         SPACE                                                                  
* CHECK TO SEE IF MAKEGOOD IS NOT MISSED, IF MISSED OK                          
         L     R1,AIOAREA2                                                      
         SR    R0,R0               LOOK FOR ANOTHER MAKEGOOD                    
         LA    R1,NUMAINEL-NUKEY(R1)  FIRST ELEMENT                             
*                                                                               
MG220    CLI   0(R1),0             IF NO MKG THAN LAST MISSED IN CHAIN          
         BE    MG240                                                            
         CLI   0(R1),X'07'                                                      
         BE    MG100               A MISSED                                     
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     MG220                                                            
         SPACE                                                                  
* IF NOT MISSED THAN CHECK ALL 06 ELEMENTS FOR LT UNIT                          
* IF IT IS LT (PROGRAM CODE/DATE/LINE #) THAN ALREADY COPIED HOPEFULLY          
MG240    L     RE,AIOAREA2                                                      
         USING NURECD,RE           LOOK FOR ANOTHER MAKEGOOD                    
         CLC   MKGSV(6),NUKPROG                                                 
         BL    MG100                                                            
         BH    EXXMOD                                                           
         CLC   MKGSV+6(2),NUKDATE                                               
         BL    MG100                                                            
         BH    EXXMOD                                                           
         CLC   MKGSV+8(1),NUKSUB                                                
         BL    MG100                                                            
         BH    EXXMOD                                                           
         DC    H'0'                DUPLICATE UNITS                              
         DROP  RE                                                               
         SPACE                                                                  
* ADD NEW MISSED UNITS - CHANGE LINE # & SAVE                                   
* ALSO CHANGE PROGRAM CODE IN MISSED ELEMENT IF NEW                             
*                                                                               
MG300    LA    R2,MKGSV                                                         
         MVC   NBAIO,AIOAREA2                                                   
MG320    CLI   0(R2),0                                                          
         BE    MG500                                                            
         MVC   NDXDA,10(R2)                                                     
         L     R4,AIOAREA2                                                      
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,0(R4)                               
*                                                                               
         MVC   PPROG,NUKPROG-NURECD(R4)                                         
         CLI   LUPVPHSW,YES                                                     
         BNE   *+10                                                             
         MVC   PPROG,SVPROG                                                     
         CLC   OLDPROG,NUMGPCOD-NUMGD(R1)                                       
         BNE   *+10                IF CHANGE PROG OPTION PROG MUST BE =         
         MVC   NUMGPCOD-NUMGD(6,R1),SVPROG                                      
         BAS   RE,BUILD                                                         
         MVC   9(1,R2),SUB                                                      
         MVC   10(4,R2),ADDKA      SAVE NEW DKA                                 
         LA    R2,MKGTBLL(R2)      NEXT MISSED                                  
         B     MG320                                                            
         SPACE                                                                  
* NOW CHANGE MAKEEGOOD ELEMENT IF DIFFERENT                                     
MG500    LA    R2,MKGSV                                                         
         MVC   NBAIO,AIOAREA1      RESET FOR NETAIO                             
MG520    CLI   0(R2),0                                                          
         BZ    EXXMOD                                                           
         MVC   NDXDA,10(R2)                                                     
         L     R4,AIOAREA2                                                      
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,0(R4)                               
*                                                                               
         LA    R1,NUMAINEL-NUKEY(R4)  FIRST ELEMENT                             
         MVI   BYTE,C'N'                                                        
*                                                                               
MG540    CLI   0(R1),0             TEST FOR EOR                                 
         BE    MG700                                                            
         CLI   0(R1),X'06'         FIND MAKEGOOD ELEMENT                        
         BE    *+12                                                             
         CLI   0(R1),X'07'         FIND MISSED ELEMENT                          
         BNE   MG560                                                            
         BAS   RE,EQUENTRY                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   8(1,RF),9(RF)       SAME NUMBER DON'T NEED TO ADD                
         BE    MG560                                                            
         MVC   NUMGSUB-NUMGD(1,R1),9(RF)                                        
         MVI   BYTE,C'Y'                                                        
*                                                                               
MG560    ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     MG540                                                            
*                                                                               
MG700    CLI   BYTE,C'Y'                                                        
         BNE   MG720                                                            
         GOTO1 AIO,DMCB,UNT+FILE+PUT,0(R4)                                      
MG720    LA    R2,MKGTBLL(R2)                                                   
         B     MG520                                                            
         EJECT                                                                  
* CHECK TO SEE IF MAKEGOOD/MISSED ELEMENTS ALREADY ADDED                        
SELMKG   LA    RF,MKGRDTBL                                                      
         LA    R0,MKGTBLN                                                       
         CLI   0(RF),0             1ST TIME DONE                                
         BZR   RE                                                               
SM100    CLI   0(RF),0                                                          
         BZ    SM200                                                            
         LR    R2,RF                                                            
         LA    RF,9(RF)                                                         
         BCT   R0,SM100                                                         
SM200    LTR   RB,RB                                                            
SMXIT    BR    RE                  TOO MANY RECORDS END                         
         EJECT                                                                  
* CHECK TO SEE IF MAKEGOOD/MISSED ELEMENTS ALREADY ADDED                        
SVMSMK   ST    RE,SAVEREG                                                       
         BAS   RE,EQUENTRY                                                      
         BZ    MMXIT                                                            
         LA    RF,MKGRDTBL                                                      
         LA    R0,MKGTBLN                                                       
MM100    CLI   0(RF),0                                                          
         BZ    MM200                                                            
         CLC   0(6,RF),NUMGPCOD-NUMGD(R1)                                       
         BNE   MM120                                                            
         CLC   6(2,RF),NUMGDATE-NUMGD(R1)                                       
         BNE   MM120                                                            
         CLC   8(1,RF),NUMGSUB-NUMGD(R1)                                        
         BE    MMXIT                                                            
MM120    LA    RF,9(RF)                                                         
         BCT   R0,MM100                                                         
         B     EXXMOD              TOO MANY RECORDS END                         
*                                                                               
MM200    MVC   0(6,RF),NUMGPCOD-NUMGD(R1)                                       
         MVC   6(2,RF),NUMGDATE-NUMGD(R1)                                       
         MVC   8(1,RF),NUMGSUB-NUMGD(R1)                                        
MMXIT    L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* CHECK TO SEE IF MAKEGOOD/MISSED ELEMENTS ALREADY ADDED                        
*                                                                               
*   --- RF IS PASSED BACK AS PARM. ---                                          
*                                                                               
EQUENTRY LA    RF,MKGSV                                                         
         LA    R0,MKGTBLN                                                       
EE100    CLI   0(RF),0                                                          
         BE    EE200                                                            
         CLC   0(6,RF),NUMGPCOD-NUMGD(R1)                                       
         BNE   EE120                                                            
         CLC   6(2,RF),NUMGDATE-NUMGD(R1)                                       
         BNE   EE120                                                            
         CLC   8(1,RF),NUMGSUB-NUMGD(R1)                                        
         BNE   EE120                                                            
         SR    R0,R0                                                            
         BR    RE                                                               
EE120    LA    RF,MKGTBLL(RF)                                                   
         BCT   R0,EE100                                                         
EE200    LTR   RB,RB                                                            
EEXIT    BR    RE                                                               
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE TO STORE DISK ADDRESS OF 1ST MAKEGOOD TO BE ADDED                 
*                                                                               
RDMDGUN1 NTR1                                                                   
         L     R4,AIOAREA2                                                      
         L     RE,AIOAREA1                                                      
         LR    R0,R4                                                            
         LA    R1,2000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   KEY,NBKEY                                                        
         LA    R3,MKGSV                                                         
         MVC   0(6,R3),KEY+(NUKPPROG-NURECD)                                    
         MVC   6(2,R3),KEY+(NUKPDATE-NURECD)                                    
         MVC   8(1,R3),KEY+(NUKPSUB-NURECD)                                     
         B     MU200                                                            
         SPACE 2                                                                
*                                                                               
* SUB-ROUTINE TO READ A MADE GOOD UNITS & STORE DISK ADDRESS                    
*        R2 = RECORD TO READ                                                    
*                                                                               
RDMDGUN  NTR1                                                                   
         LA    R3,MKGSV            FIND NEXT ENTRY POSTION                      
         LA    R0,MKGTBLN                                                       
MU100    CLI   0(R3),0                                                          
         BZ    MU120                                                            
         LA    R3,MKGTBLL(R3)                                                   
         BCT   R0,MU100                                                         
         B     MUBAD               TOO MANY FOR TABLE                           
*                                                                               
MU120    MVC   0(9,R3),0(R2)                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(3),NBACTAM    AGY/MED/CLI                                  
         MVC   KEY+4(4),NBACTNET                                                
         MVC   KEY+8(6),0(R2)                                                   
         MVC   KEY+14(2),6(R2)                                                  
         MVC   KEY+16(1),NBACTEST                                               
         MVC   KEY+17(1),8(R2)                                                  
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(18),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA2                                                      
         GOTO1 AIO,DMCB,UNT+FILE+GET,0(R4)                                      
*                                                                               
MU200    SR    R0,R0               LOOK FOR BUILD OR PAYED                      
         LA    R1,NUMAINEL-NUKEY(R4)  FIRST ELEMENT                             
*                                                                               
MU220    CLI   0(R1),0             TEST FOR EOR                                 
         BE    MU300                                                            
         CLI   0(R1),X'10'                                                      
         BE    MU280                                                            
         CLI   0(R1),X'12'                                                      
         BNE   MU260                                                            
MU240    CLI   LUPVPHSW,YES                                                     
         BNE   MU260                                                            
         CLC   OLDPROG,NUMGPCOD-NUMGD(R1)                                       
         BNE   MUBAD               IF CHANGE PROG OPTION PROG MUST BE =         
MU260    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     MU220                                                            
* CHECK IF BILLED ELEMENT HAS BEEN UNBILLED                                     
MU280    DS    0H                                                               
         USING NUBILD,R1                                                        
         TM    NUBILST,X'20'                                                    
         BZ    MUBAD                                                            
         B     MU260                                                            
         DROP  R1                                                               
*                                                                               
MU300    MVC   10(4,R3),KEY+21                                                  
*                                                                               
MUGOOD   SR    R0,R0                                                            
         B     EXXMOD                                                           
*                                                                               
MUBAD    LTR   R4,R4                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
                                                                                
*                                                                               
* FORMAT SHORT HAND DATE INPUT: WILL USE ESITMATE DATES TO DETERMINE            
*                               DEFUALT YEAR FOR INPUT                          
*                                                                               
DATEFOR  NTR1                                                                   
         CLI   TEMPDATE,C'T'       CHECK FOR INVALID FORMAT FOR THIS            
         BNL   ERROR               TYPE OF INPUT                                
         ZIC   R4,FLDH+5           GET LEGNTH OF INPUT                          
         LA    R4,TEMPDATE(R4)     USE R4 AS A INDEX TO THE LENGTH              
         MVC   1(2,R4),=C'00'      DEFAULT YEAR IS 00                           
         MVI   0(R4),C'/'          INPUT SLASH                                  
         GOTO1 VDATVAL,DMCB,(0,TEMPDATE),WORKDATE                               
         CLC   =F'0',DMCB          VALIDATE DATE INPUT                          
         BE    ERROR                                                            
*                                                                               
         CLC   WMONTH,ESTSTART+2   COMPARE INPUT MONTH/ESTIMATE MONTH           
         BL    DF100               USE END YEAR AS DEFAULT                      
         BH    DF090               USE START YEAR AS DEFAULT                    
*                                                                               
         CLC   WDAY,ESTSTART+4     DETERMINE IF THE MONTH IS THE SAME           
         BL    DF100                                                            
DF090    MVC   WYEAR,ESTSTART      MOVE DEFAULT START YEAR                      
         B     DF110                                                            
DF100    MVC   WYEAR,ESTEND        MOVE DEFAULT END YEAR                        
DF110    GOTO1 VDATCON,DMCB,(0,WORKDATE),(5,TEMPDATE)                           
         XIT1                                                                   
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
         BNE   CKRSN20             NO, REASON CODE NOT INPUTTED                 
*                                                                               
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    CKRSN20             NO, REASON CODE NOT INPUTTED                 
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
                                                                                
CKRSN20  MVI   FERN,AUDITERR                                                    
         L     RE,NBAIO                                                         
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'02'      IS REASON CODE REQUIRED                      
         BO    CKRSN30             YES                                          
         OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    CKRSNX                                                           
         B     ERROR                                                            
CKRSN30  OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ERROR                                                            
                                                                                
CKRSNX   B     EXXMOD                                                           
         EJECT                                                                  
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
DEMOMTCH DC    C'** ERROR - UNMATCHED DEMO BASE IN PACKAGES **'                 
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
       ++INCLUDE NEBUYF9D                                                       
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    0D                                                               
SVTOPACK DS    X                                                                
SSTADATE DS    XL4                                                              
SVFRCLT  DS    XL2                                                              
SVFREST  DS    X                                                                
SVFRPACK DS    X                                                                
SVFRPRD  DS    CL3                                                              
SVFRPRN  DS    X                                                                
SVPROG   DS    CL6                                                              
SVPGDYTM DS    CL5                 SAVE PROGRAM DAY/TIME                        
SVUNITS  DS    F                                                                
*                                                                               
SVPGUNTS DS    F                                                                
         DS    0D                                                               
SVNBLOCK DS    XL(NEBLOCKL)                                                     
SVPRDLST DS    CL2000              PRDS THAT HAVE THIS EST                      
SVDATAL  EQU   *-SVDATA                                                         
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
CNTUNITS DS    F                                                                
DATES    DS    0XL4                                                             
ENDATE   DS    XL2                 FILTER END DATE                              
STADATE  DS    XL2                 FILTER START DATE                            
*                                                                               
WORKDATE DS    0XL6                DATE USED FOR VALIDATION                     
WYEAR    DS    XL2                                                              
WMONTH   DS    XL2                                                              
WDAY     DS    XL2                                                              
*                                                                               
TEMPDATE DS    CL8                                                              
*                                                                               
UNITDA   DS    XL4                 SAVED DISK ADDRESS                           
FRCLT    DS    XL2                 PACKED FROM CLIENT CODE                      
FREST    DS    X                   FROM ESTIMATE                                
FRPACK   DS    X                                                                
FRPRD    DS    CL3                 FROM PRODUCT CODE                            
FRPRN    DS    X                   FROM PRODUCT NUMBER                          
FRLEN    DS    X                   FROM LENGTH                                  
FRSTTYPE DS    X                   FROM STATION TYPE                            
FRPTTYPE DS    X                   FROM POSTING TYPE                            
FRSUBMED DS    X                   FROM SUB MEDIA                               
TOPACK   DS    X                                                                
TOPAKCTL DS    X                   TO PACKAGE CONTROL                           
COPYSW   DS    C                                                                
FRSTSW   DS    C                                                                
SUB      DS    X                                                                
ALPRD    DS    CL1                 SAVE ALLOCATED PRDS                          
ALTRAF   DS    CL1                 TRANSFER TRAFFIC                             
LUPVPHSW DS    CL1                 LOOK UP NEW DEMO'S                           
NPRDCDE  DS    CL3                 NEW PRD TO READ EST FOR                      
FSTREAD  DS    CL1                 1ST READ DON'T DO PRG CHECK                  
OLDPROG  DS    CL6                 IF CHANGING PROGRAM OLD PROGRAM              
PPROG    DS    CL6                 PROGRAM PARIMETER                            
ELEM     DS    CL80                ELEMENT BUILD AREA                           
PRDLST   DS    CL220               LIST OF PRODUCTS TO CHECK ESTS               
ADDKA    DS    F                   LAST DISK ADDRESS ADDED                      
MKGTBLL  EQU   14                  LENGTH OF ENTIES INTO TABLE                  
MKGTBLN  EQU   40                  NUMBER OF ENTRIES ALLOWED IN TABLE           
FULLREAD DS    CL1                 Y MEANS 100 RECORDS PROCESSED                
PKGRCASH DS    CL2                 CASH PCTG FROM PACKAGE RECORD                
PKGRTRAD DS    CL2                 TRADE PCTG FROM PACKAGE RECORD               
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
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
*  DDOFFICED                                                                    
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE NETBILLRD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022NEBUY16   12/13/10'                                      
         END                                                                    
