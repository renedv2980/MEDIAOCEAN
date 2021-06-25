*          DATA SET SPNWS38    AT LEVEL 078 AS OF 05/14/09                      
*PHASE T20738A,*                                                                
         TITLE 'BWS38 - BUYERS WORK SHEET - REVISION ESTIMATING SCREEN'         
T20738   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20738**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R0,AXTRAN           SET EXTENTION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         A     R1,APRELO                                                        
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         ST    R6,LASAVE                                                        
         AHI   R6,SAVAREAX-SAVAREA+2048                                         
******** L     R6,ATIA             USE TIA FOR SAVED VALUES                     
         USING SAVED,R6                                                         
*                                                                               
         LA    R2,IOKEY            R2=A(HEADER KEY)                             
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY         R3=A(DETAIL KEY)                             
         USING BWDRECD,R3                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         DC    AL4(0)                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     FSTRPT                                                           
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   MVI   LCHG,0                                                           
         MVI   APINDS,0                                                         
         XC    BWHKEY,BWHKEY       BUILD HEADER KEY                             
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         GOTO1 AVALMED,RESMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,RESBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD                          
         BZ    VALK1                                                            
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   VALKX                                                            
*                                                                               
VALK1    GOTO1 AVALCAM,RESNUMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALKX                                                            
         MVC   BWHKCAM,BCAM                                                     
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BE    *+16                                                             
         LA    R1,RESNUMH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
         GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,ESTDEMS          SAVE ONLY RATINGS AND IMPRESSIONS            
         LA    RE,LESTDEMS                                                      
         XC    LESTDEMS,LESTDEMS                                                
*                                                                               
VALK2    OC    0(3,R1),0(R1)                                                    
         BZ    VALK6                                                            
         CLI   1(R1),C'R'                                                       
         BE    VALK4                                                            
         CLI   1(R1),C'E'                                                       
         BE    VALK4                                                            
         CLI   1(R1),C'I'                                                       
         BE    VALK4                                                            
         CLI   1(R1),X'21'         IS IT USER DEFINED DEMO?                     
         BNE   *+14                                                             
VALK4    MVC   0(3,RE),0(R1)                                                    
         LA    RE,3(RE)                                                         
         LA    R1,3(R1)                                                         
         B     VALK2                                                            
*                                                                               
VALK6    CLI   INODEM,0            TEST DEMO OPTION SET                         
         BE    VALK22                                                           
         TM    INODEM,X'80'        YES-TEST FOR NEXT                            
         BZ    VALK12                                                           
         NI    INODEM,FF-X'80'     YES                                          
         ZIC   RE,INODEM           RE=1/2/3/4                                   
         BCTR  RE,0                                                             
         MHI   RE,12                                                            
         LA    RE,LESTDEMS(RE)     RE=A(REQUESTED DEMOS)                        
         OC    0(3,RE),0(RE)       TEST BEYOND END OF LIST                      
         BNZ   *+8                                                              
         LA    RE,LESTDEMS         YES-START FROM FIRST DEMO                    
         LA    R0,4                MAX 4 DEMOS                                  
         LA    R1,LESTDEMS         MOVE DEMOS TO BEGINNING OF DEMO LIST         
*                                                                               
VALK8    MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         OC    0(3,RE),0(RE)                                                    
         BZ    VALK10                                                           
         BCT   R0,VALK8                                                         
*                                                                               
VALK10   LR    RF,R1               CLEAR REST OF DEMO LIST                      
         LA    RE,LESTDEMS                                                      
         SR    RF,RE                                                            
         LA    RE,L'LESTDEMS                                                    
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
         OI    INODEM,X'80'        RESET DEMO OPTION                            
         B     VALK22                                                           
*                                                                               
VALK12   LA    R1,INODEM+1         VALIDATE DEMOS IN THE DEMO OPTION            
         ZIC   R0,INODEM                                                        
         MVI   APBYTE,0                                                         
         MVC   APFULL(3),ESTDEMS                                                
         OC    INORTG,INORTG                                                    
         BZ    VALK14                                                           
         MVC   APFULL(3),INORTG    APFULL(3)=TARGET RATING                      
*                                                                               
VALK14   LA    RE,LESTDEMS                                                      
*                                                                               
VALK16   OC    0(3,RE),0(RE)                                                    
         BZ    EDEM                                                             
         CLC   1(2,RE),1(R1)                                                    
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     VALK16                                                           
         CLI   APBYTE,0                                                         
         BNE   VALK18                                                           
         CLC   APFULL+1(2),1(R1)                                                
         BNE   VALK18                                                           
         MVI   APBYTE,1            APBYTE=1 IF TARGET IS IN LIST                
*                                                                               
VALK18   LA    R1,3(R1)                                                         
         BCT   R0,VALK14                                                        
*                                                                               
         IC    R0,INODEM           SET THE DEMO LIST FROM DEMO OPTION           
         LA    R1,INODEM+1                                                      
         LA    RE,LESTDEMS                                                      
         XC    LESTDEMS,LESTDEMS                                                
         CLI   APBYTE,0            TEST TARGET ALREADY IN LIST                  
         BNE   VALK20                                                           
         MVC   0(3,RE),APFULL      NO-SET FIRST DEMO = TARGET                   
         LA    RE,3(RE)                                                         
*                                                                               
VALK20   MVC   0(3,RE),0(R1)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,VALK20                                                        
*                                                                               
VALK22   XC    DBLOCK,DBLOCK       GET DEMO NAMES                               
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    R1,LESTDEMS                                                      
         MVI   APBYTE,NDEMOS                                                    
         ICM   R1,8,APBYTE                                                      
         ST    R1,APPARM                                                        
         LA    R0,ESTUSRNM                                                      
         XC    LDNAMES(7*NDEMOS),LDNAMES                                        
         GOTO1 VDEMOCON,APPARM,,(2,LDNAMES),(C'S',DBLOCK),(R0)                  
*                                                                               
         GOTO1 AVALSTA,RESSTAH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
****                                                                            
         LA    R2,IOKEY            BUILD HEADER POINTER TO FIND                 
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM                                                     
         MVC   BWHKMKT,BMKT                                                     
*                                                                               
         GOTO1 AIO,DIRHI                                                        
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BNE   ECMSEQ                                                           
         MVC   BCMSEQ,BWHKSEQ                                                   
*                                                                               
         MVI   BBUYLINE,0                                                       
         MVI   BSEQNUM,0                                                        
*                                                                               
         LA    R2,RESLINH          VALIDATE LINE NUMBER                         
         ST    R2,FVADDR           SO WE POINT TO CORRECT FIELD ON ERR          
         CLI   5(R2),0                                                          
         BNE   VALK24                                                           
         CLI   RESSEQH+5,0                                                      
         BNE   VALK26                                                           
         LR    R1,R2               SET R1 TO A(FIELD HDR) FOR ENOC              
         B     ENOC                MISSING INPUT FILED                          
*                                                                               
VALK24   TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    EIIF                NO                                           
         CLI   APACTN,ACTADD       ADDING                                       
         BE    EIIF                NO GOOD, THEY SHOULD USE SEQ                 
*                                                                               
         ZIC   R1,RESLINH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,RESLIN(0)                                                  
         CVB   R1,APDUB                                                         
         CHI   R1,256                                                           
         BH    EIIF                                                             
         LTR   R1,R1                                                            
         BZ    EIIF                                                             
         STC   R1,BBUYLINE         SAVE BINARY LINE NUMBER                      
         B     VALK30                                                           
*                                                                               
VALK26   LA    R2,RESSEQH                                                       
         CLI   5(R2),0                                                          
         BE    VALK30                                                           
*                                                                               
         CLI   RESLINH+5,0                                                      
         BNE   EIIF                CAN'T HAVE BOTH LINE AND SEQ NUMBERS         
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    EIIF                NO                                           
         ZIC   R1,RESSEQH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,RESSEQ(0)                                                  
         CVB   R1,APDUB                                                         
         CHI   R1,256                                                           
         BH    EIIF                                                             
         LTR   R1,R1                                                            
         BZ    EIIF                                                             
         STC   R1,BSEQNUM          SAVE BINARY LINE NUMBER                      
*                                                                               
VALK30   LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         CLI   BBUYLINE,0          IF NO BUYLINE                                
         BE    VALK40              THEN CK FOR MANUALLY ADDED REVISION          
*                                                                               
         USING BUYKEY,R2                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,CMPCLTC                                                  
         MVC   BUYKPRD,CMPPRDN                                                  
         MVC   BUYMSTA,BMKT        THIS ALSO INCLUDES THE STATION               
         MVC   BUYKEST,CMPESTN                                                  
         MVC   BUYKBUY+1(1),BBUYLINE                                            
*                                                                               
VALK30A  GOTO1 AIO,DIRHI+IO2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(BUYKBUY-BUYKEY),IOKEYSAV                                   
         BNE   VALK99                                                           
*                                                                               
         CLI   BUYKBUY,0           REG, PB ACTV, OR POL BUY?                    
         BNE   VALK32              NO                                           
         CLC   BBUYLINE,BUYKBUY+1  YES, SAME LINE NUMBER?                       
         BNE   VALK99                                                           
         B     VALK34                                                           
*                                                                               
VALK32   CLI   BUYKBUY,X'FF'       POL-BRND?                                    
         BNE   VALK99                                                           
         CLC   BBUYLINE,BUYKBUY+2  YES, SAME LINE NUMBER?                       
         BE    VALK34                   YES                                     
         BL    VALK99                                                           
         MVC   BUYKBUY+2(1),BBUYLINE    NO, SET IT TO THE SAME LINE #           
         B     VALK30A                      AND TRY GETTING KEY NOW             
*                                                                               
VALK34   MVC   APRECKEY,IOKEY                                                   
***      MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         GOTO1 AIO,FILGET2                                                      
         CLI   IOKEY,NBRKTYPQ                                                   
         BH    VALK36                                                           
         L     R0,AIOAREA3                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA2                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     VALK50                                                           
***********************************                                             
* GET ASSOCIATED BUY REVISION RECORD FOR THIS BUYLINE                           
***********************************                                             
VALK36   XC    IOKEY,IOKEY                                                      
         USING NBRKEY,R2                                                        
         MVI   NBRKTYP,NBRKTYPQ    READ NWS BUY REVISION RECORD                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,BSTA                                                     
         MVC   NBRKKBUY,APRECKEY+BUYKBUY-BUYKEY                                 
         NI    LINDS,X'FF'-LADDREC                                              
*                                                                               
         GOTO1 AIO,DIRHI+IO2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BE    VALK34                                                           
         OI    LINDS,LADDREC                                                    
******   GOTO1 =A(SETUPNBR),RR=APRELO                                           
         GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
         B     VALK50                                                           
***********************************************************************         
* TO GET THE MANUALLY ADDED BUY REVISION RECORDS (NO BUY LINE ENTERED)          
***********************************************************************         
         USING NBRKEY,R2                                                        
VALK40   MVI   NBRKTYP,NBRKTYPQ    READ NWS BUY REVISION RECORD                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,BSTA                                                     
         MVC   NBRKNBSQ,BSEQNUM                                                 
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,DIRHI                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BE    VALK34                                                           
         NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL-APIOKRES-APIOKADD           
*                                                                               
         TM    TWAMODE,TWAMLSM     AND IN LIST/SELECT MODE                      
         BO    VALK99              YES-RETURN ERROR CODE                        
         B     VALKX                                                            
*                                                                               
VALK50   OI    APINDS,APIOKDIS     SET INDICATOR TO RECORD FOUND                
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         GOTO1 AVALDAY,RESDAYH     VALIDATE DAYS                                
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALTIM,RESTIMH     VALIDATE TIMES                               
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALDPL,RESDLNH     VALIDATE DAYPART/LENGTH                      
         BNE   VALKX                                                            
*                                                                               
         LA    RE,NBRSCST1                                                      
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MONITOR ACTIVE              
         BZ    VALK51                                                           
         CLI   APRECNUM,RECSID     AND NOT SID RECORD                           
         BE    VALK51                                                           
         TM    APRECID,RIEFFDT2    YES-APRECID(1)=RECORD INDICATOR              
         BZ    *+8                                                              
         LA    RE,NBRSCST2                                                      
         TM    APRECID,RIEFFDT3                                                 
         BZ    VALK51                                                           
         LA    RE,NBRSCST3                                                      
*                                                                               
VALK51   ST    RE,LACOST           SAVE A(COST FIELD)                           
*                                                                               
         XC    LADEMEL,LADEMEL                                                  
         XC    LAUPGEL,LAUPGEL                                                  
         XC    LAODTEL,LAODTEL                                                  
         XC    LASHPEL,LASHPEL                                                  
         LR    R8,R3                                                            
*                                                                               
VALK52   CLI   0(R8),0                                                          
         BE    VALKX                                                            
         CLI   0(R8),NBRDMELQ                                                   
         BNE   VALK52K                                                          
         ST    R8,LADEMEL          A(DEMO ELEMENT)                              
         LR    R1,R8                                                            
         XR    R0,R0                                                            
         IC    R0,1(R8)                                                         
         AR    R8,R0               R8 POINTS TO NEXT ELEMENT NOW                
         LA    R1,2(R1)                                                         
VALK52E  CR    R1,R8                                                            
         BNL   VALK52               - WE'RE DONE WITH CONVERTING                
***  2 DECIMAL                                                                  
         LR    R0,R1               SAVE OFF R1                                  
         ST    R1,APPARM                                                        
         GOTO1 AADJPREC,APPARM     CONVERTING ALL DEMOS IF NEEDS BE             
         LR    R1,R0               RESTORE R1                                   
         LA    R1,8(R1)            NEXT DEMO                                    
         B     VALK52E                                                          
***  2 DECIMAL                                                                  
*                                                                               
VALK52K  CLI   0(R8),NBRUPELQ                                                   
         BNE   *+8                                                              
         ST    R8,LAUPGEL          A(UPGRADE ELEMENT)                           
         CLI   0(R8),NBRODELQ                                                   
         BNE   *+8                                                              
         ST    R8,LAODTEL          A(OVERRIDE ELEMENT)                          
         CLI   0(R8),NBRSHELQ                                                   
         BNE   *+8                                                              
         ST    R8,LASHPEL          A(SHARE/PUT OVERRIDE ELEMENT)                
         XR    R0,R0               LOCATE ELEMENTS IN DETAIL RECORD             
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VALK52                                                           
*                                                                               
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
         LA    R1,RESMEDH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
*                                                                               
VALKX    B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
FSTRPT   L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         MVI   TWADEM,FF           INITIALIZE DEMO POINTER                      
         SR    R0,R0                                                            
         LA    R4,RESDAYH          TURN PREV VALIDATED BITS ON                  
*                                                                               
FSTR2    TM    FVATRB-FVIHDR(R4),FVAPROT                                        
         BO    *+12                                                             
         OI    FVIIND-FVIHDR(R4),FVIVAL                                         
         OI    6(R4),X'80'                                                      
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   FSTR2                                                            
*                                                                               
         XC    SVUPFILE,SVUPFILE   CLEAR SAVED VALUES                           
         XC    SVUPGRD,SVUPGRD                                                  
         XC    SVUPFRBK,SVUPFRBK                                                
         MVI   SVUPFRBT,0                                                       
         XC    SVUPFBL,SVUPFBL                                                  
         XC    SVUPINP,SVUPINP                                                  
         XC    SVUPDAY,SVUPDAY                                                  
         XC    SVUPTIM,SVUPTIM                                                  
         XC    SVUPPUT,SVUPPUT                                                  
         XC    SVUPSHR,SVUPSHR                                                  
         XC    SVBKS,SVBKS                                                      
         XC    SVBKTPS,SVBKTPS                                                  
         XC    SVWKY,SVWKY                                                      
         MVI   SVRTGSVC,0                                                       
*                                                                               
         OC    CMPUP,CMPUP         TEST FOR CAMPAIGN UPGRADE                    
         BZ    FSTR4                                                            
         MVC   SVUPFILE,CMPUF                                                   
         MVC   SVUPGRD,CMPUP                                                    
         MVC   SVUPFRBK,CMPFB                                                   
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   FSTR3G                                                           
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUPFRBT,CMPFBTP    SVUPFRBK BOOKTYPE                            
FSTR3G   MVC   SVUPFBL,CMPFBLST                                                 
         MVC   SVUPINP,CMPUPIN                                                  
         MVC   SVUPPUT,CMPUPUT                                                  
         MVC   SVUPSHR,CMPUSHR                                                  
*                                                                               
FSTR4    ICM   R4,15,LAUPGEL       UPGRADE ELEMENT                              
         BZ    FSTR6                                                            
         USING NBRUPELD,R4                                                      
         MVC   SVUPFILE,NBRUPFIL   DETAIL UPGRADE VALUES                        
         MVC   SVUPGRD,NBRUPEXP                                                 
         MVC   SVUPFRBK,NBRUPOBK                                                
**       MVC   SVUPFRBK,NBRSBOOK                                                
         TM    NBRUPOBK+1,BTY2CHAR                                              
         BNO   FSTR4G                                                           
         CLI   NBRUPLEN,NBRUPLNQ                                                
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   NBRUPOBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUPFRBT,NBRUPOBT                                                
FSTR4G   XC    SVUPFBL,SVUPFBL                                                  
         CLI   NBRUPLEN,51                                                      
         BL    *+10                                                             
         MVC   SVUPFBL,NBRUPBKL                                                 
         MVC   SVUPINP,NBRUPINP                                                 
         MVC   SVUPPUT,NBRSUPUT                                                 
         MVC   SVUPSHR,NBRSUSHR                                                 
*                                                                               
FSTR6    ICM   R4,15,LAODTEL                                                    
         BZ    FSTR8                                                            
         USING NBRODELD,R4                                                      
         MVC   SVUPDAY,NBRODODY    OVERRIDE DAY/TIME                            
         MVC   SVUPTIM,NBRODOTM                                                 
*                                                                               
FSTR8    BRAS  RE,WEEKLY           INSPECT THE WEEKLY FIELD                     
         BNE   FSTRX                                                            
         BRAS  RE,COST             INSPECT THE COST FIELD                       
         BL    FSTRX                                                            
         XC    EBLOCK,EBLOCK       EDIT THE COST                                
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBFLOAT,C'$'                                                     
         MVC   EBAIN,LACOST                                                     
         LA    R1,RESCST                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'RESCST                                                  
         MVI   EBDECS,2                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    RESCSTH+6,FVOXMT                                                 
*                                                                               
         BAS   RE,VALUPGRD         INSPECT UPGRADE FIELD                        
         BL    FSTRX                                                            
         BE    FSTR10                                                           
         CLI   QMED,C'R'        IS IT RADIO? LOGIC FOLLOWS LIKE SPNWS04         
         BE    FSTR9             - YUP, SKIP CHECK                              
         OC    SVUPINP,SVUPINP     ANY UPGRADE FORMULA?                         
         BZ    ENOUPG               - NOPE, ERROR                               
FSTR9    MVC   RESUPG,SVUPINP      NONE - EDIT UPGRADE EXPRESSION               
         OI    RESUPGH+6,FVOXMT                                                 
*                                                                               
FSTR10   BAS   RE,BOOKS            INSPECT BOOK FIELDS                          
         BNE   FSTRX                                                            
         OC    SVBKS,SVBKS                                                      
         BNZ   FSTR14              BOOKS INPUT                                  
         OC    MKTLKUP,MKTLKUP     TEST POSSIBILITY OF MARKET OVERRIDE          
         BZ    FSTR12                                                           
         MVC   LDEMLST,=X'00D901FF' YES-CALL SPDEMUP TO GET RTG SERVICE         
*                                                                               
         MVI   QBOOKTYP,0                                                       
         TM    SVUPFRBK+1,BTYBITSQ SPECIAL BOOK?                                
         BZ    FSTR11                                                           
         TM    SVUPFRBK+1,BTY2CHAR   2 CHARACTER BOOKTYPE?                      
         BNO   FSTR10R              - NOPE                                      
         CLI   SVUPFRBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QBOOKTYP,SVUPFRBT                                                
         B     FSTR11                                                           
*                                                                               
FSTR10R  GOTO1 AGETBKTY,APPARM,(C'B',SVUPFRBK+1),QBOOKTYP                       
****10T  NI    SVUPFRBK+1,X'FF'-BTYBITSQ                                        
*                                                                               
FSTR11   GOTO1 ASDEMUP                                                          
*                                                                               
FSTR12   GOTO1 AGETBKS             GET THE BOOKS                                
*                                                                               
FSTR14   BRAS  RE,DISBKS           DISPLAY THE BOOKS                            
*                                                                               
         MVC   RESPRG,NBRSPROG                                                  
         OI    RESPRGH+6,FVOXMT                                                 
*                                                                               
         LA    R0,NDEMOS           BUILD DEMO LIST                              
         LA    R4,SVDEMS                                                        
         LA    R8,LESTDEMS                                                      
*                                                                               
FSTR34   OC    0(3,R8),0(R8)                                                    
         BZ    FSTR38                                                           
         MVC   0(3,R4),0(R8)                                                    
         MVC   3(3,R4),0(R8)                                                    
         MVC   6(3,R4),0(R8)                                                    
         CLI   1(R8),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R8),C'E'                                                       
         BNE   *+16                                                             
         MVI   4(R4),C'S'                                                       
         MVI   7(R4),C'P'                                                       
         B     FSTR36                                                           
         CLI   1(R8),C'I'                                                       
         BNE   *+16                                                             
         MVI   4(R4),C'X'                                                       
         MVI   7(R4),C'Q'                                                       
         B     FSTR36                                                           
         CLI   1(R8),X'21'         USER INPUT DEMO NAME?                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FSTR36   LA    R4,9(R4)                                                         
         LA    R8,3(R8)                                                         
         BCT   R0,FSTR34                                                        
*                                                                               
FSTR38   MVI   0(R4),FF                                                         
         XC    LDEMOVR,LDEMOVR                                                  
         GOTO1 ADEMUP              DO THE UPGRADES                              
****  CANADA DOESN'T NEED TO OVERRIDE THE RATING SERVICE                        
         CLI   CUDMED,C'C'         TEST CANADA                                  
         BE    *+10                 - YUP, SKIP THE OVERRIDE                    
****  CANADA DOESN'T NEED TO OVERRIDE THE RATING SERVICE                        
         MVC   SVRTGSVC,LRTGSVC    SAVE THE RATING SERVICE FROM SPDEMUP         
         GOTO1 AGETDEMS            GET THE DEMO VALUES                          
*                                                                               
         LA    R4,LESTDEMS         FIND NUMBER OF ESTIMATE DEMOS                
         SR    RE,RE                                                            
*                                                                               
FSTR40   OC    0(3,R4),0(R4)                                                    
         BZ    *+16                                                             
         LA    RE,1(RE)                                                         
         LA    R4,3(R4)                                                         
         B     FSTR40                                                           
         STC   RE,SVDEMNO                                                       
*                                                                               
         OI    RESCMTH+6,FVOXMT    FORMAT BOTTOM LINE                           
         XC    RESCMT,RESCMT                                                    
         MVC   RESCMT(L'CMT1),CMT1                                              
         OI    RESCMT1H+6,FVOXMT                                                
         XC    RESCMT1,RESCMT1                                                  
         MVC   RESCMT1(L'CMT1A),CMT1A                                           
         LA    R4,RESCMT1+L'CMT1A+1                                             
         CLI   APRECNUM,RECSID                                                  
         BNE   *+14                                                             
         MVC   0(L'CMT2,R4),CMT2                                                
         LA    R4,L'CMT2+1(R4)                                                  
         TM    TWAMODE,TWAMLSM                                                  
         BZ    FSTRX                                                            
         MVC   0(L'CMT3,R4),CMT3                                                
*                                                                               
*STRX    GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),SAVED                         
FSTRX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DISKEY   MVC   IOKEY,APRECKEY      GET THE RECORD                               
         GOTO1 AIO,DIRHI+IO2                                                    
         BNE   DISKX                                                            
*                                                                               
         CLC   IOKEY(L'BUYKEY),IOKEYSAV                                         
         BNE   DISKX                                                            
*                                                                               
         GOTO1 AIO,FILGET2                                                      
         BNE   DISKX                                                            
*                                                                               
         L     R3,AIOAREA2                                                      
         CLI   0(R3),NBRKTYPQ                                                   
         BH    *+12                                                             
         USING NBRKEY,R3                                                        
         LA    R1,NBRKAGMD                                                      
         B     DISK05                                                           
         USING BUYKEY,R3                                                        
         MVC   APBYTE,BUYKAM                                                    
         OC    APBYTE,BBYRMASK                                                  
         LA    R1,APBYTE                                                        
*                                                                               
DISK05   GOTO1 AGETMED                                                          
         BNE   *+10                                                             
         MVC   RESMED,QMED         MEDIA                                        
*                                                                               
         LA    R1,BBYR                                                          
         ICM   R1,8,=X'01'                                                      
         GOTO1 AGETBYR                                                          
         MVC   RESBYR,QBYR         BUYER                                        
*                                                                               
         GOTO1 AGETCM,BCMSEQ       CAMPAIGN/MARKET                              
         MVC   RESNUM,QCAM                                                      
*                                                                               
         XC    APWORK,APWORK                                                    
         CLI   0(R3),NBRKTYPQ                                                   
         BH    *+12                                                             
         USING NBRKEY,R3                                                        
         LA    R1,NBRKSEQ          SEQ/STA TO SIMULATE MARKET/STA               
         B     *+8                                                              
         USING BUYKEY,R3                                                        
         LA    R1,BUYMSTA                                                       
         MVC   APWORK(L'BUYMSTA),0(R1)                                          
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,APWORK+9                  
*                                                                               
         MVC   RESSTA(5),APWORK+9  STATION                                      
         OI    RESSTAH+6,X'80'                                                  
         MVI   RESSTAH+5,5                                                      
         CLI   RESSTA+4,C' '                                                    
         BE    *+16                                                             
         CLI   RESSTA+4,C'T'                                                    
         BNE   DISK10                                                           
         MVI   RESSTA+4,C' '                                                    
         MVI   RESSTAH+5,4                                                      
*                                                                               
DISK10   CLI   RESSTA,C'0'                                                      
         BL    *+14                                                             
         MVI   RESSTA+4,C'/'                                                    
         MVC   RESSTA+5(3),APWORK+9+5                                           
*                                                                               
         CLI   0(R3),NBRKTYPQ                                                   
         BH    DISK20                                                           
         USING NBRKEY,R3                                                        
         ZIC   R1,NBRKNBSQ         SEQ NUMBER FOR MANUAL BUY REVISION           
         LA    R2,RESSEQH                                                       
         CLI   NBRKNBSQ,0                                                       
         BNE   DISK25                                                           
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         IC    R1,NBRSBYLN                                                      
         LA    R2,RESLINH                                                       
         DROP  RE                                                               
         B     DISK25                                                           
         USING BUYKEY,R3                                                        
DISK20   ZIC   R1,BUYKBUY          BUYLINE NUMBER FOR AGENCY BUY RECORD         
         LA    R2,RESLINH                                                       
*                                                                               
DISK25   CVD   R1,APDUB                                                         
         UNPK  8(3,R2),APDUB                                                    
         OI    8+2(R2),X'F0'                                                    
         MVI   5(R2),3                                                          
         OI    4(R2),X'08'         VALID NUMERIC                                
         OI    6(R2),X'80'                                                      
*********                                                                       
* DISPLAY THE DAYS                                                              
*********                                                                       
         CLI   0(R3),NBRKTYPQ      BUY REVISION RECORD?                         
         BH    DISK30                                                           
         USING NBRKEY,R3                                                        
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         LA    R1,NBRSDAYS                                                      
         B     DISK35                                                           
         DROP  RE                                                               
         USING BUYKEY,R3                                                        
DISK30   LA    R1,BDDAY                                                         
         DROP  R3                                                               
DISK35   GOTO1 AGETDAY                                                          
         MVC   RESDAY,QDAYS                                                     
*********                                                                       
* DISPLAY THE TIMES                                                             
*********                                                                       
         CLI   0(R3),NBRKTYPQ      BUY REVISION RECORD?                         
         BH    DISK40                                                           
         USING NBRKEY,R3                                                        
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         LA    R1,NBRSTIMS                                                      
         B     DISK45                                                           
         DROP  RE                                                               
         USING BUYKEY,R3                                                        
DISK40   LA    R1,BDTIMST                                                       
         DROP  R3                                                               
DISK45   GOTO1 AGETTIM                                                          
         MVC   RESTIM,QTIMES                                                    
*********                                                                       
* DISPLAY THE DAYPART AND SPOT LENGTH  **  AND THE SUBDAYPART!!!  MHC           
*********                                                                       
         XC    RESSUB,RESSUB       DON'T KNOW IF WE NEED IT YET                 
         MVI   RESSDP,0                                                         
         CLI   0(R3),NBRKTYPQ      BUY REVISION RECORD?                         
         BNH   DISK50               - YES IT IS!                                
*                                                                               
***  WE'RE CALLING SETUPNBR SO WE CAN GET THE SUBDAYPART!                       
         GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
*                                                                               
         L     R3,AIOAREA3         NEED R3 TO POINT TO AIO3                     
***  R3 IS POINTING TO AIO2 IF WE ALREADY HAVE NBR RECORD   MHC                 
*                                                                               
         USING NBRKEY,R3                                                        
DISK50   LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         MVC   RESDLN(1),NBRSDYPT                                               
*                                                                               
         MVC   RESSDP,NBRSSBDP     SAVE OFF THE SUBDAYPART AS WELL              
         CLI   RESSDP,C' '         DOES IT HAVE ANYTHING?                       
         BNH   *+10                 - NOPE, DON'T MOVE IN SUBDPT WORD           
         MVC   RESSUB(10),=C'Subdaypart'                                        
*                                                                               
         ZIC   RF,NBRSSLN                                                       
         DROP  RE                                                               
         DROP  R3                                                               
*                                                                               
DISK55   BAS   RE,DISLEN                                                        
         MVC   RESTIM,QTIMES                                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DISKX    XIT1                                                                   
BLANKS   DC    32C' '                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORDS                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SAVED,R6                                                         
DISREC   L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    DISR2                                                            
         CLI   APPFKEY,PFK04       AND PF4, PF5, PF6 OR PF12                    
         BL    DISR2                                                            
         CLI   APPFKEY,PFK06                                                    
         BNH   *+12                                                             
         CLI   APPFKEY,PFK12                                                    
         BNE   DISR2                                                            
         MVI   APMODE,APMLRP       YES-TELL ROOT ITS THE LAST SCREEN            
         MVC   SCPFKEY,APPFKEY         AND PASS ON PF KEY VALUE                 
         MVI   TWAFLAG,0                                                        
         B     DISRX                                                            
*                                                                               
DISR2    CLI   APPFKEY,PFK02       TEST PF KEY 2 AND COME FROM                  
         BNE   *+12                COMPETITION SCREEN                           
         TM    TWAFLAG,TWAFCOM                                                  
         BO    DISR10              YES - TRANSFER SID TO BWS                    
*                                                                               
         CLI   TWADEM,FF           TEST DISPLAYED AT LEAST ONE SCREEN           
         BE    DISR14              NO-CANNOT SWAP YET                           
         TM    TWAFLAG,TWAFCOM     TEST WE'VE NOT COME FROM COMP SCREEN         
         BO    DISR8                                                            
         LA    R1,RESSELH          SEE IF CURSOR IS IN A SELECT FLD OR          
         SR    R4,R4               SOMETHING ENTERED IN A SELECT FIELD          
         LA    R8,RESDM2H-RESDM1H                                               
         LA    R9,RESCMTH                                                       
*                                                                               
DISR4    GOTO1 AFVAL               VALIDATE SELECT FIELD                        
         BH    DISRX                                                            
         BL    *+16                                                             
         CLI   FVIFLD,C'*'         SOMETHING ENTERED - TEST FOR *               
         BE    *+8                 YES - ALREADY SELECTED                       
         B     DISR6               NO - SELECT THIS DEMO                        
         CLC   FVABSA,ACCURS       TEST CURSOR IS IN SELECT FIELD               
         BE    DISR6               YES - SELECT THIS DEMO                       
         LA    R4,1(R4)                                                         
         BXLE  R1,R8,DISR4                                                      
         B     DISR8                                                            
*                                                                               
DISR6    MVI   8(R1),C'*'          DEMO SELECTED - MARK WITH A *                
         OI    FVATRB-FVIHDR(R1),FVAHIGH                                        
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         ZIC   RF,TWADEM           DETERMINE WHICH DEMO IS SELECTED             
         MHI   RF,9                                                             
         LA    RF,SVDEMS(RF)                                                    
         MHI   R4,9                                                             
         AR    RF,R4                                                            
         MVC   TWADEMO,0(RF)                                                    
         B     DISR12                                                           
*                                                                               
DISR8    CLI   APPFKEY,PFK01       TEST PF1                                     
         BNE   DISR10                                                           
         MVI   APPFKEY,0                                                        
         TM    TWAFLAG,TWAFCOM     YES - TEST COME FROM COMP SCREEN             
         BZ    *+12                                                             
         BAS   RE,RESCOM                 YES - RESTORE COMP SCREEN              
         B     DISRX2                                                           
         CLI   TWADEM,0                  NO - SWAP WITH TARGET DEMO             
         BNE   *+16                                                             
         MVI   RESSEL,C'*'               MARK TARGET DEMO WITH A *              
         OI    RESSELH+FVATRB-FVIHDR,FVAHIGH                                    
         OI    RESSELH+FVOIND-FVIHDR,FVOXMT                                     
         MVC   TWADEMO,INORTG                                                   
         OC    INORTG,INORTG                                                    
         BNZ   DISR12                                                           
         MVC   TWADEMO,SVDEMS                                                   
         B     DISR12                                                           
*                                                                               
DISR10   CLI   APPFKEY,PFK02       TEST PF KEY 2                                
         BNE   DISR14                                                           
         CLI   APRECNUM,RECSID                                                  
         BE    DISR14                                                           
         MVI   APPFKEY,0           IGNORE IT IF NOT SID                         
         B     DISR14                                                           
*                                                                               
         SPACE 1                                                                
* SWAP TO COMPETITION SCREEN                                                    
*                                                                               
DISR12   GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(3,0),TWAD                          
         MVI   APMODE,APMSWP                                                    
         OI    TWAFLAG,TWAFEST                                                  
         NI    TWAFLAG,FF-TWAFRET                                               
         B     DISRX                                                            
         SPACE 1                                                                
*                                                                               
DISR14   TM    TWAFLAG,TWAFRET     TEST RETURN FROM COMP SCREEN                 
         BZ    *+12                                                             
         NI    TWAFLAG,FF-TWAFRET  YES-JUST EXIT                                
         B     DISRX                                                            
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
         TM    RESWKYH+FVIIND-FVIHDR,FVIVAL   TEST WEEKLY OPTION CHANGE         
         BO    DISR15                                                           
         OI    RESWKYH+FVIIND-FVIHDR,FVIVAL   YES -                             
         BRAS  RE,WEEKLY           INSPECT THE WEEKLY FIELD                     
         BNE   FSTRX                                                            
*                                                                               
DISR15   NI    LFLAG,255-LNEWDAYS-LDAYOVR    INSPECT DAYS FIELD                 
         MVC   APHALF,BDPT                                                      
         MVC   APFULL,BTIMES                                                    
         MVC   APBYTE,BDAYS                                                     
         GOTO1 AVALDAY,RESDAYH                                                  
         BNE   DISRX                                                            
         MVC   BDPT(2),APHALF                                                   
         MVC   BTIMES,APFULL                                                    
         CLC   BDAYS,NBRSDAYS      TEST DAYS = THE RECORD'S DAYS                
         BE    *+8                                                              
         OI    LFLAG,LDAYOVR       NO-INDICATE DAYS ARE OVERRIDDEN              
         TM    RESDAYH+FVIIND-FVIHDR,FVIVAL    TEST DAYS FIELD CHANGED          
         BO    *+12                                                             
         OI    RESDAYH+FVIIND-FVIHDR,FVIVAL    YES -                            
         OI    LFLAG,LNEWDAYS      INDICATE DAYS CHANGED THIS TIME              
         MVC   LDAYS,BDAYS         SAVE THE DAYS                                
         MVC   BDAYS,APBYTE                                                     
*                                                                               
         NI    LFLAG,255-LNEWTIME-LTIMOVR    INSPECT TIMES FIELD                
         MVC   APHALF,BDPT                                                      
         MVC   APFULL,BTIMES                                                    
         GOTO1 AVALTIM,RESTIMH                                                  
         BNE   DISRX                                                            
         MVC   BDPT(2),APHALF                                                   
         CLC   BTIMES,NBRSTIMS     TEST TIMES = THE RECORD'S TIMES              
         BE    *+8                                                              
         OI    LFLAG,LTIMOVR       NO-INDICATE TIMES ARE OVERRIDDEN             
         TM    RESTIMH+FVIIND-FVIHDR,FVIVAL    TEST TIMES FIELD CHANGED         
         BO    *+12                                                             
         OI    RESTIMH+FVIIND-FVIHDR,FVIVAL    YES -                            
         OI    LFLAG,LNEWTIME      INDICATE TIMES CHANGED THIS TIME             
         MVC   LTIMES,BTIMES       SAVE THE TIMES                               
         MVC   BTIMES,APFULL                                                    
*                                                                               
DISR16   TM    RESCSTH+FVIIND-FVIHDR,FVIVAL    TEST COST FIELD CHANGED          
         BO    DISR17                                                           
         OI    RESCSTH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BRAS  RE,COST                         INSPECT COST FIELD               
         BL    DISRX                                                            
         BE    DISR17                                                           
         L     RE,LACOST                                                        
         OC    0(L'NBRSCST1,RE),0(RE)          COST FIELD BLANK                 
         BZ    DISR17                                                           
         XC    0(L'NBRSCST1,RE),0(RE)                                           
         OI    LCHG,LCOST                                                       
*                                                                               
DISR17   TM    RESUPGH+FVIIND-FVIHDR,FVIVAL    TEST UPGRADE CHANGED             
         BO    DISR20                                                           
         OI    RESUPGH+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,VALUPGRD                                                      
         BL    DISRX                                                            
         BE    DISR20                                                           
         OC    LAUPGEL,LAUPGEL     UPGRADE REMOVED - TEST FOR UPGRD ELE         
         BZ    DISR18                                                           
         OI    LCHG,LUPG           YES - INDICATE UPGRADE CHANGE                
         MVC   SVUPFILE,CMPUF            USE CAMPAIGN UPGRADE VALUES            
         MVC   SVUPGRD,CMPUP                                                    
         MVC   SVUPFRBK,CMPFB                                                   
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   DISR17G                                                          
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUPFRBT,CMPFBTP    SVUPFRBK BOOKTYPE                            
DISR17G  MVC   SVUPFBL,CMPFBLST                                                 
         MVC   SVUPINP,CMPUPIN                                                  
*                                                                               
         MVI   APELEM,NBRUPELQ           DELETE UPGRADE ELEMENT                 
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         GOTO1 ADELELS,NBRRECD                                                  
         XC    LAUPGEL,LAUPGEL                                                  
         MVI   APELEM,NBRODELQ           DELETE OVERRIDE ELEMENT                
         GOTO1 ADELELS,NBRRECD                                                  
         XC    LAODTEL,LAODTEL                                                  
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
DISR18   CLI   QMED,C'R'        IS IT RADIO? LOGIC FOLLOWS LIKE SPNWS04         
         BE    DISR19            - YUP, SKIP CHECK                              
         OC    CMPUPIN,CMPUPIN     DO WE HAVE A CAMPAIGN UPGRADE?               
         BZ    ENOUPG               - NOPE, ERROR!!                             
DISR19   MVC   RESUPG,CMPUPIN            DISPLAY CAMPAIGN UPGRADE               
         OI    RESUPGH+6,FVOXMT                                                 
*                                                                               
DISR20   TM    LCHG,LUPG           TEST FOR UPGRADE CHANGE                      
         BO    *+12                                                             
         TM    LFLAG,LNEWTIME+LNEWDAYS  OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR22                                                           
         GOTO1 ADEMUP              YES - DO THE UPGRADES                        
         CLC   LRTGSVC,SVRTGSVC    TEST RATING SERVICE CHANGED                  
         BE    DISR22                                                           
         OI    LCHG,LBK            YES-GET DEMOS AGAIN                          
         MVC   SVRTGSVC,LRTGSVC                                                 
*                                                                               
DISR22   TM    RESBK1H+FVIIND-FVIHDR,FVIVAL    TEST ANY BOOK CHANGES            
         BZ    DISR24                                                           
         TM    RESBK2H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR24                                                           
         TM    RESBK3H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR24                                                           
         TM    RESBK4H+FVIIND-FVIHDR,FVIVAL                                     
         BZ    DISR24                                                           
         TM    LFLAG,LGETBKS       NO-TEST NEED TO GET BOOKS AGAIN              
         BZ    DISR25              NO                                           
         GOTO1 AGETBKS             YES-GET THE BOOKS                            
         BRAS  RE,DISBKS               AND DISPLAY THEM                         
         B     DISR25                                                           
*                                                                               
*ISR24   OI    RESBK1H+FVIIND-FVIHDR,FVIVAL                                     
*        OI    RESBK2H+FVIIND-FVIHDR,FVIVAL                                     
*        OI    RESBK3H+FVIIND-FVIHDR,FVIVAL                                     
*        OI    RESBK4H+FVIIND-FVIHDR,FVIVAL                                     
DISR24   BAS   RE,BOOKS            INSPECT THE BOOK FIELDS                      
         BNE   DISRX                                                            
         TM    SVWKYIND,SVONEBK    TEST SINGLE BOOK, 4 WEEKS                    
         BZ    DISR25                                                           
         BRAS  RE,DISBKS           YES-MAKE SURE BOOKS ARE DISPLAYED            
*                                                                               
DISR25   TM    LCHG,LBK            TEST FOR ANY BOOK CHANGE                     
         BO    *+12                                                             
         TM    LFLAG,LNEWTIME+LNEWDAYS  OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR26                                                           
         GOTO1 AGETDEMS            YES-GET THE DEMO VALUES                      
         OC    SVWKY,SVWKY         TEST WEEKLY OPTION                           
         BZ    DISR26              NO                                           
         BRAS  RE,DISBKS           YES-REDISPLAY THE BOOKS                      
*                                                                               
DISR26   TM    RESPRGH+FVIIND-FVIHDR,FVIVAL   TEST FOR PROGRAMMING              
         BO    DISR30                         CHANGE                            
         OI    RESPRGH+FVIIND-FVIHDR,FVIVAL                                     
         GOTO1 AFVAL,RESPRGH                                                    
         BH    DISRX                                                            
         CLI   FVIFLD,C'='         TEST PROGRAM ADJACENCY CODE                  
         BNE   DISR28                                                           
         GOTO1 AVALADJ,FVIFLD+1    YES - VALIDATE                               
         BNE   DISRX                                                            
         CLC   NBRSADJC,QADJCD     VALID - TEST CHANGE                          
         BE    DISR30                                                           
         MVC   NBRSADJC,QADJCD     YES - SET ADJACENCY CODE                     
         OI    LCHG,LPRG                 INDICATE RECORD CHANGE                 
         B     DISR30                                                           
*                                                                               
DISR28   CLC   NBRSPROG,FVIFLD      TEST PROGRAM CHANGE                         
         BE    DISR30                                                           
         MVC   NBRSPROG,FVIFLD                                                  
         OI    NBRSINDS,NBRSIPRG                                                
         OI    LCHG,LPRG                                                        
*                                                                               
DISR30   BAS   RE,DISSRC           DISPLAY THE RATING SERVICE                   
*                                                                               
         CLI   TWADEM,FF           FOR VERY FIRST SCREEN, SKIP TO               
         BE    DISR31                                     DISPLAY LOGIC         
         GOTO1 AVALRSP             VALIDATE RATING AND SHARE/PUT FIELDS         
         BNE   DISRX                                                            
*                                                                               
DISR31   CLI   TWADEM,FF           TEST FOR VERY FIRST SCREEN                   
         BNE   *+12                                                             
         MVI   TWADEM,0                                                         
         B     DISR32                                                           
         CLI   APPFKEY,PFK02       NO - TEST SID TRANSFER                       
         BE    DISR54                                                           
*                                  NO - TEST FOR CHANGES THAT WILL              
         TM    LCHG,LCOST+LUPG+LBK+LPROJ  AFFECT THE NUMBERS ON SCREEN          
         BNZ   DISR32                    YES- RE-DISPLAY CURRENT SCREEN         
         TM    LFLAG,LNEWTIME+LNEWDAYS   SAME FOR NEW DAYS/TIMES                
         BNZ   DISR32                                                           
         ZIC   RE,TWADEM                 NO - DISPLAY NEXT SCREEN               
         LA    RF,NDEMSCR                                                       
         AR    RE,RF                                                            
         STC   RE,TWADEM                                                        
         CLC   TWADEM,SVDEMNO      TEST ALL SCREENS DISPLAYED                   
         BL    DISR32                                                           
         MVI   TWADEM,0            YES - DISPLAY FROM FIRST SCREEN              
         MVI   APMODE,APMLRP             TELL CONTROLLER                        
         TM    TWAFLAG,TWAFCOM           TEST COME FROM COMP SCREEN             
         BZ    DISR32                                                           
         BAS   RE,RESCOM                 YES - RESTORE COMP SCREEN              
         B     DISRX2                                                           
*                                                                               
DISR32   SR    RE,RE               CLEAR THE SCREEN                             
         LA    R4,RESDM1H                                                       
         LA    R8,RESCMTH                                                       
*                                                                               
DISR34   IC    RE,0(R4)                                                         
         SHI   RE,9                                                             
         BNM   *+6                                                              
         DC    H'0'                TWA MUST BE FUCKED                           
         EX    RE,DRCLC                                                         
         BE    DISR36                                                           
         EX    RE,DROC                                                          
         BZ    DISR36                                                           
         EX    RE,DRXC                                                          
         OI    6(R4),FVOXMT                                                     
*                                                                               
DISR36   LA    R4,9(RE,R4)                                                      
         CR    R4,R8                                                            
         BL    DISR34                                                           
*                                                                               
         LA    R0,NDEMSCR          FORMAT DEMO NAMES                            
         ZIC   RE,TWADEM                                                        
         ZIC   RF,SVDEMNO                                                       
         LTR   RF,RF               TEST ANY DEMOS AT ALL                        
         BZ    DISR54              NO-SKIP                                      
         SR    RF,RE                                                            
         CR    R0,RF               TEST FOR A FULL SCREEN                       
         BNH   *+6                                                              
         LR    R0,RF               NO                                           
         STC   R0,LNDEMSCR         NUMBER OF DEMOS ON THIS SCREEN               
         MHI   RE,7                                                             
         LA    RE,LDNAMES(RE)                                                   
         LA    R1,RESDM1H                                                       
         LA    R4,RESLN1H                                                       
         LA    R8,RESLN2H                                                       
*                                                                               
DISR38   OC    0(7,RE),0(RE)                                                    
         BZ    DISR40                                                           
         MVC   8(L'RESDM1,R1),0(RE)                                             
         OI    6(R1),FVOXMT                                                     
         OI    6(R4),FVOXMT        TRANSMIT BOTH DEMO LINES                     
         OI    6(R8),FVOXMT                                                     
         LA    R1,RESDM2H-RESDM1H(R1)                                           
         LA    R4,RESDM2H-RESDM1H(R4)                                           
         LA    R8,RESDM2H-RESDM1H(R8)                                           
         LA    RE,7(RE)                                                         
         BCT   R0,DISR38                                                        
*                                                                               
DISR40   XC    EBLOCK,EBLOCK       FORMAT THE BOOK VALUES                       
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    R0,4                                                             
         ZIC   R4,TWADEM                                                        
         MHI   R4,12                                                            
         LA    R4,SVDEMVAL(R4)     R4 = A(RTG/SHR/PUT)                          
*                                                                               
DISR42   ZIC   R9,LNDEMSCR                                                      
         LA    R2,RESLN1                                                        
         USING LINE1D,R2                                                        
*                                                                               
DISR44   LNR   RE,R0               FORMAT THE RATING                            
         AHI   RE,3                                                             
         LA    R8,L1RTG1                                                        
         MVI   EBLOUT,L'L1RTG1                                                  
         LTR   RE,RE                                                            
         BM    DISR45                                                           
         MH    RE,=Y(L1RTG3-L1RTG2)                                             
         LA    R8,L1RTG2(RE)                                                    
         MVI   EBLOUT,L'L1RTG2                                                  
DISR45   ST    R8,EBAOUT                                                        
         ST    R4,EBAIN                                                         
         MVI   EBDECS,1                                                         
***  2 DECIMAL  ***                                                             
         TM    0(R4),DMODEM2D      WE NEED 2 DECIMAL?                           
         BNO   *+12                                                             
         MVI   EBDECS,2                                                         
         NI    0(R4),FF-DMODEM2D   TAKE OFF THE BIT FOR CORRECT VALUE           
***  2 DECIMAL  ***                                                             
**       OC    0(4,R4),0(R4)                                                    
**       BNZ   DISR45K                                                          
**       MVC   0(10,R8),=C'      *0.0'   YES - EDIT MYSELF                      
**       LTR   RE,RE                                                            
**       BM    *+10                                                             
**       MVC   0(12,R8),=C'        *0.0'                                        
***  2 DECIMAL  ***                                                             
**       CLI   EBDECS,2            ARE WE DOING 2 DECIMALS?                     
**       BNE   DISR45X              - NOPE                                      
**       MVC   0(10,R8),=C'     *0.00'  YES-EDIT MYSELF                         
**       LTR   RE,RE                                                            
**       BM    *+10                                                             
**       MVC   0(12,R8),=C'       *0.00'                                        
**       B     DISR45X                                                          
*                                                                               
DISR45K  MVI   EBSCIN,0                                                         
         ZIC   RE,EBLOUT                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R8),0(R8)                                                    
         OC    0(4,R4),0(R4)                                                    
         BZ    DISR46                                                           
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
***  2 DECIMAL  ***                                                             
DISR45X  CLI   EBDECS,2                                                         
         BNE   *+8                                                              
         OI    0(R4),DMODEM2D      TURN THE 2 DECIMAL X'40' BIT BACK ON         
***  2 DECIMAL  ***                                                             
*                                                                               
DISR46   LA    R4,4(R4)            FORMAT THE SHARE/PUT                         
         LA    R2,RESLN2-RESLN1(R2)                                             
         USING LINE2D,R2                                                        
         LNR   R8,R0                                                            
         LA    R8,4(R8)                                                         
         MH    R8,=Y(L2SHP2-L2SHP1)                                             
         LA    R8,L2SHP1(R8)                                                    
         XC    0(L'L2SHP1,R8),0(R8)                                             
         LA    R8,2(R8)                                                         
         BAS   RE,FMTSHP           FORMAT SHARE/PUT ROUTINE                     
         LA    R4,8(R4)                                                         
         LA    R2,RESLN3-RESLN2(R2)                                             
         BCT   R9,DISR44           DO FOR ALL DEMOS                             
*                                                                               
         LNR   R4,R0                                                            
         LA    R4,5(R4)                                                         
         MH    R4,=Y(NDEMOS*12)                                                 
         LA    R4,SVDEMVAL(R4)                                                  
         ZIC   RE,TWADEM                                                        
         MHI   RE,12                                                            
         AR    R4,RE                                                            
         BCT   R0,DISR42           DO FOR ALL BOOKS                             
*                                                                               
         LA    R8,RESRTGH          FORMAT PROJECTED VALUES                      
         ZIC   R4,TWADEM                                                        
         MHI   R4,12                                                            
         LA    R4,SVPROJ(R4)                                                    
         ZIC   R9,LNDEMSCR                                                      
*                                                                               
DISR48   MVI   EBLOUT,L'RESRTG     FORMAT PROJECTED RATING                      
         LA    RE,L'FVIHDR(R8)                                                  
         ST    RE,EBAOUT                                                        
         LA    RE,APFULL                                                        
         ST    RE,EBAIN                                                         
         XC    0(L'RESRTG,RE),0(RE)                                             
         MVC   APFULL,0(R4)                                                     
         LA    R1,APFULL                                                        
***  2 DECIMAL  ***                                                             
*        ST    R1,APPARM                                                        
*        GOTO1 AADJPREC,APPARM,,   SEE IF WE NEED TO CONVERT                    
***  2 DECIMAL  ***                                                             
         NI    APFULL,FF-X'80'                                                  
         MVI   EBFLOAT,0                                                        
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         MVI   EBDECS,1                                                         
***  2 DECIMAL  ***                                                             
         TM    APFULL,DMODEM2D     WE NEED 2 DECIMAL?                           
         BNO   *+12                                                             
         MVI   EBDECS,2                                                         
         NI    APFULL,FF-DMODEM2D   TAKE OFF THE BIT FOR CORRECT VALUE          
***  2 DECIMAL  ***                                                             
**       OC    APFULL,APFULL       TEST DEMO = 0                                
**       BNZ   DISR49                                                           
**       MVC   8(10,R8),=C'      *0.0'   YES - EDIT MYSELF                      
***  2 DECIMAL  ***                                                             
**       CLI   EBDECS,2            ARE WE DOING 2 DECIMALS?                     
**       BNE   *+10                 - NOPE                                      
**       MVC   8(10,R8),=C'     *0.00'  YES-EDIT MYSELF                         
**       B     DISR50                                                           
*                                                                               
DISR49   MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR50   OI    6(R8),FVOXMT                                                     
         MVI   EBLOUT,L'RESCPP     FORMAT THE CPP/CPM                           
         LA    R8,RESCPPH-RESRTGH(R8)                                           
         LA    RE,L'FVIHDR(R8)                                                  
         ST    RE,EBAOUT                                                        
         XC    0(L'RESCPP,RE),0(RE)                                             
         L     R1,LACOST                                                        
         ICM   RE,15,0(R1)                                                      
         BZ    DISR52                                                           
         OC    APFULL,APFULL                                                    
         BZ    DISR52                                                           
         SRDA  RE,32                                                            
         M     RE,=F'20'                                                        
***  2 DECIMAL  ***                                                             
         TM    0(R4),DMODEM2D      IS IT 2 DECIMALS?                            
         BNO   *+8                                                              
         M     RE,=F'10'           MULTIPLY BY 10                               
***  2 DECIMAL  ***                                                             
         D     RE,APFULL                                                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         ST    RF,APFULL                                                        
         MVI   EBDECS,2                                                         
         MVI   EBFLOAT,0                                                        
         CLC   APFULL,=F'100000'                                                
         BL    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'82'                                                     
         MVI   EBFLOAT,C'$'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR52   OI    6(R8),FVOXMT                                                     
         LA    R8,RESSHPH-RESCPPH+8(R8)                                         
         LA    R4,4(R4)                                                         
         BAS   RE,FMTSHP           FORMAT SHR/PUT FOR PROJECTED                 
         SHI   R8,8                                                             
         OI    6(R8),FVOXMT                                                     
         LA    R4,8(R4)                                                         
         LA    R8,RESLN3H-RESSHPH(R8)                                           
         LA    R8,RESRTGH-RESLN1H(R8)                                           
         BCT   R9,DISR48           DO FOR ALL DEMOS                             
*                                                                               
DISR54   CLI   APRECNUM,RECSID     TEST SID RECORD                              
         BNE   DISR56                                                           
         L     R0,LASAVE           YES - SAVE BWS RECORD THAT'S BEEN            
         AH    R0,=Y(SVREC-SAVAREA)      BUILT FROM IT                          
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA2                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   APPFKEY,PFK02       TEST SID TRANSFER                            
         BNE   DISRX                                                            
         MVI   APPFKEY,0                                                        
         GOTO1 AXFRADD             YES - TRANSFER SID TO BWS                    
         BNE   DISRX                                                            
         MVI   SCPFKEY,PFK05             AND RETURN TO LIST SCREEN              
         MVI   TWAFLAG,0                                                        
         MVI   APMODE,APMLRP                                                    
         B     DISRX                                                            
*                                                                               
DISR56   TM    LCHG,LCOST+LUPG+LPRG+LPROJ   TEST FOR ANY DETAIL RECORD          
         BZ    DISR58                       CHANGES                             
*                                                                               
         NI    LINDS,X'FF'-LRECDEL-LADDREC                                      
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'NBRKEY),0(R3)                                            
         GOTO1 AIO,DIRHI+IO1+IORDEL                                             
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BE    DISR56A             NEED TO ADD                                  
         OI    LINDS,LADDREC                                                    
         B     DISR57                                                           
*                                                                               
DISR56A  NI    LINDS,X'FF'-LRECDEL                                              
         GOTO1 AIO,FILGETU1+IORDEL                                              
         BE    DISR57                                                           
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OI    LINDS,LRECDEL                                                    
*                                                                               
DISR57   MVC   IOKEY(L'NBRKEY),0(R3)                                            
         LA    R1,FILADD3                                                       
         TM    LINDS,LADDREC                                                    
         BNZ   *+8                                                              
         LA    R1,FILPUT3                                                       
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    LINDS,LRECDEL                                                    
         BZ    DISR58                                                           
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'NBRKEY),NBRKEY                                           
*                                                                               
         GOTO1 AIO,DIRRDUP+IORDEL                                               
         BE    DISR58                                                           
         TM    IOERR,IOEDEL        RECORD IS DELETED?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,IOKEY                                                         
         NI    NBRKCNTL,X'FF'-X'80'    DELETE KEY                               
         GOTO1 AIO,DIRWRT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DISR58   B     DISRX                                                            
*                                                                               
*ISRX    GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),SAVED                         
DISRX    DS    0H                                                               
*                                                                               
DISRX2   TM    TWAMODE,TWAMLSM     TEST LIST/SELECT ACTIVE                      
         BZ    *+8                                                              
         MVI   TWALSACT,ACTEST     YES-LAST SELECT ACTION = ESTIMATE            
         CLI   APMODE,APMSWP                                                    
         BE    *+12                                                             
         CLI   APMODE,APMRET                                                    
         BNE   EXIT                                                             
         MVI   APPARM,RECBUY       FOR SWAP, RECORD = BUYS                      
         MVI   APPARM+1,ACTCOM               ACTION = COMPETITION               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         SPACE 2                                                                
DRCLC    CLC   8(0,R4),SPACES      EXECUTED INSTRUCTIONS                        
DROC     OC    8(0,R4),8(R4)                                                    
DRXC     XC    8(0,R4),8(R4)                                                    
*                                                                               
DISLEN   CVD   RF,APDUB            DISPLAY SPOT LENGTH                          
         OI    APDUB+7,X'0F'                                                    
         LA    R1,RESDLN+1                                                      
         CLI   RESDLN,C'1'                                                      
         BL    *+12                                                             
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         UNPK  0(3,R1),APDUB                                                    
         CLI   0(R1),C'0'                                                       
         BNE   *+14                                                             
         MVC   0(2,R1),1(R1)                                                    
         MVI   2(R1),C' '                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RATING SERVICE                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSRC   LR    R0,RE                                                            
         MVI   APBYTE,0            CLEAN UP APBYTE JUST IN CASE                 
         L     RE,ATWA                                                          
         CLI   APBYTE,0                                                         
         AHI   RE,SVMALPHA-TWAD                                                 
         USING SVMALPHA,RE                                                      
         CLI   SVMRTGSV,C'0'       NSI?                                         
         BNE   *+8                  - NOPE                                      
         MVI   APBYTE,C'N'                                                      
         CLI   SVMRTGSV,C'1'       BBM?                                         
         DROP  RE                                                               
         BNE   *+8                  - NOPE                                      
         MVI   APBYTE,C'A'                                                      
         CLI   APBYTE,0                                                         
         BNE   DISSRC10                                                         
***                                                                             
         MVC   APBYTE,SVRTGSVC                                                  
         CLI   APBYTE,0                                                         
         BNE   *+10                                                             
         MVC   APBYTE,CLTSRC                                                    
DISSRC10 OI    RESSRCH+6,FVOXMT                                                 
***  CABLE/FUSION                                                               
         MVC   RESSRC,=C'FUS'                                                   
         CLI   APBYTE,C'F'                                                      
         BE    DISSRCX                                                          
***  CABLE/FUSION                                                               
         MVC   RESSRC,=C'NSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    *+10                                                             
         MVC   RESSRC,=C'ARB'                                                   
         CLI   CUDMED,C'C'                                                      
         BNE   DISSRCX                                                          
         MVC   RESSRC,=C'CSI'                                                   
         CLI   APBYTE,C'N'                                                      
         BE    DISSRCX                                                          
         MVC   RESSRC,=C'BBM'                                                   
*                                                                               
DISSRCX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT SHARE/PUT                                         *         
* INPUT  : R4 = A(SHR/PUT)                                            *         
*          R8 = A(FORMAT AREA)                                        *         
***********************************************************************         
         SPACE 1                                                                
FMTSHP   NTR1  ,                                                                
         CLI   CLTBWPRO+15,C'Y'    TEST SUPPRESS SHARES AND PUTS                
         BE    FSHPX                                                            
         MVC   APDUB,0(R4)                                                      
         LA    R9,APDUB                                                         
         NI    0(R9),255-X'80'                                                  
         NI    4(R9),255-X'80'                                                  
         MVI   EBLOUT,5            DETERMINE L'SHARE FIELD                      
         CLC   0(4,R9),=F'1000'                                                 
         BNL   FSHP6                                                            
         MVI   EBLOUT,4                                                         
         TM    0(R4),X'80'                                                      
         BZ    FSHP2                                                            
         CLC   0(4,R9),=F'100'                                                  
         BL    FSHP6                                                            
         MVI   EBLOUT,5                                                         
         B     FSHP6                                                            
*                                                                               
FSHP2    CLC   0(4,R9),=F'100'                                                  
         BL    FSHP4                                                            
         TM    4(R4),X'80'                                                      
         BZ    FSHP6                                                            
         CLC   4(4,R9),=F'100000'                                               
         BL    FSHP6                                                            
*                                                                               
FSHP4    MVI   EBLOUT,3                                                         
*                                                                               
FSHP6    MVI   APBYTE,0                                                         
         ZIC   R2,EBLOUT                                                        
         LNR   R2,R2                                                            
         LA    R2,9(R2)            R2=MAX L'PUT FIELD                           
         LA    R1,1                DETERMINE ACTUAL L'PUT FIELD                 
         TM    4(R4),X'80'         AND WHETHER TO ROUND THE PUT                 
         BO    *+8                                                              
         LA    R1,10                                                            
         CLI   EBLOUT,5                                                         
         BE    FSHP8                                                            
         MHI   R1,10                                                            
         CLI   EBLOUT,4                                                         
         BE    FSHP8                                                            
         MHI   R1,10                                                            
*                                                                               
FSHP8    LA    R0,4                                                             
         LA    RE,APWORK                                                        
         ST    R1,0(RE)                                                         
         MHI   R1,10                                                            
         LA    RE,4(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         CLC   4(4,R9),APWORK+12                                                
         BL    *+12                                                             
         MVI   APBYTE,1                                                         
         B     FSHP10                                                           
         CLC   4(4,R9),APWORK+8                                                 
         BL    *+12                                                             
         MVI   APBYTE,1                                                         
         B     FSHP9                                                            
         CLC   4(4,R9),APWORK+4                                                 
         BNL   FSHP10                                                           
         CLC   APWORK+4(4),=F'10'                                               
         BE    FSHP10                                                           
         CLC   4(4,R9),APWORK                                                   
         BNL   FSHP9                                                            
         CLC   APWORK(4),=F'10'                                                 
         BE    FSHP9                                                            
         BCTR  R2,0                                                             
         LA    R8,1(R8)                                                         
         CLC   APWORK(4),=F'1000'                                               
         BNE   FSHP9                                                            
         CLC   4(4,R9),=F'100'                                                  
         BNL   FSHP9                                                            
         BCTR  R2,0                                                             
         LA    R8,1(R8)                                                         
*                                                                               
FSHP9    BCTR  R2,0                DECREASE L'PUT FIELD BY ONE                  
         LA    R8,1(R8)            SHIFT SHARE FIELD ONE TO RIGHT               
*                                                                               
FSHP10   MVI   EBFLOAT,0           FORMAT THE SHARE                             
         CLC   0(4,R9),=F'1000'                                                 
         BNL   *+16                                                             
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         OC    0(4,R9),0(R9)                                                    
         BNZ   FSHP12                                                           
         MVC   0(1,R8),EBFLOAT                                                  
         LR    R1,R8                                                            
         CLI   EBFLOAT,0                                                        
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   0(3,R1),=C'0.0'                                                  
         B     FSHP20                                                           
*                                                                               
FSHP12   ST    R8,EBAOUT                                                        
         ST    R9,EBAIN                                                         
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
FSHP20   ZIC   RE,EBLOUT           FORMAT THE PUT                               
         AR    R8,RE                                                            
         MVI   0(R8),C'/'                                                       
         LA    R4,4(R4)                                                         
         LA    R9,4(R9)                                                         
         ST    R9,EBAIN                                                         
         LA    R8,1(R8)                                                         
         MVI   EBFLOAT,0                                                        
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         OC    0(4,R9),0(R9)                                                    
         BNZ   FSHP22                                                           
         MVC   0(1,R8),EBFLOAT                                                  
         LR    R1,R8                                                            
         CLI   EBFLOAT,0                                                        
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVC   0(3,R1),=C'0.0'                                                  
         B     FSHPX                                                            
*                                                                               
FSHP22   ST    R8,EBAOUT                                                        
         STC   R2,EBLOUT                                                        
         CLI   APBYTE,0                                                         
         BE    *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
FSHPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE COMPETITION SCREEN                                          *         
***********************************************************************         
         SPACE 1                                                                
RESCOM   NTR1                                                                   
         NI    TWAFLAG,FF-TWAFCOM                                               
******** GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(4,0),SAVED  SAVE TIA               
         L     R4,ATIA                                                          
         GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(3,0),(R4)                           
         LA    R0,BWSLAST                                                       
         LA    RE,BWSLAST-TWAD(R4)                                              
         LH    R1,=Y(TWASVLEN)                                                  
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R1,BWSMSGH          TRANSMIT THE SCREEN                          
         LH    RF,=Y(TWASVLEN)                                                  
         LA    RF,BWSLAST(RF)                                                   
         SR    RE,RE                                                            
         OI    6(R1),FVOXMT                                                     
         ICM   RE,1,0(R1)                                                       
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                                                             
         MVI   1(R1),1                                                          
         MVI   2(R1),1                                                          
         MVI   APMODE,APMRET       PASS RETURN MODE                             
         OI    TWAFLAG,TWAFRET     RETURN BIT                                   
         ICM   R1,15,AINP                                                       
         BZ    EXIT                                                             
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSCRN                                                
         MVI   TIOBCNT,X'E4'                                                    
         MVI   TIOBCNT+1,0                                                      
         DROP  R1                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE COST FIELD                                              *         
* OUTPUT : LCHG = LCOST IF COST FIELD IN RECORD CHANGED               *         
*          CC EQ  - COST FOUND AND RECORD CHANGED IF NECESSARY        *         
*             LO  - BAD ERROR                                         *         
*             HI  - COST NOT FOUND                                    *         
***********************************************************************         
         SPACE 1                                                                
COST     NTR1                                                                   
         GOTO1 AFVAL,RESCSTH                                                    
         BH    COSTX2                                                           
         BL    COSTX1                                                           
         ZIC   RF,FVILEN                                                        
         ST    RF,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   APPARM,FF                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVICST)                                             
         B     COSTX2                                                           
         GOTO1 ANETCOST,APPARM+4   NET DOWN THE COST IF NECESSARY               
         L     RE,LACOST                                                        
         CLC   APPARM+4(4),0(RE)                                                
         BE    COSTX2                                                           
         OI    LCHG,LCOST                                                       
         MVC   0(4,RE),APPARM+4                                                 
         B     COSTX2                                                           
*                                                                               
COSTX1   CR    RA,RB                                                            
         B     EXIT                                                             
*                                                                               
COSTX2   CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT UPGRADE FIELD                                               *         
* OUTPUT : LCHG = LUPG IF UPGRADE IN RECORD CHANGED                   *         
*          CC EQ  - UPGRADE FOUND AND RECORD CHANGED IF NECESSARY     *         
*             LO  - BAD ERROR                                         *         
*             HI  - UPGRADE NOT FOUND                                 *         
***********************************************************************         
         SPACE 1                                                                
VALUPGRD NTR1                                                                   
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         CLI   RESUPGH+5,0         TEST UPGRADE ENTERED                         
         BE    UPGRX1                                                           
         MVI   APFLAG,X'F8'                                                     
         GOTO1 AVALUPG,RESUPGH     VALIDATE UPGRADE FIELD                       
         BNE   UPGRX2              ERROR                                        
         ICM   R4,15,LAUPGEL       TEST FOR UPGRADE ELEM                        
         BZ    UPGR2                                                            
         USING NBRUPELD,R4                                                      
         CLC   NBRUPFIL,APWORK     YES - TEST FOR CHANGES                       
         BNE   UPGR4                                                            
         CLC   NBRUPEXP,APWORK+1                                                
         BNE   UPGR4                                                            
         CLC   NBRUPOBK,APWORK+9                                                
         BNE   UPGR4                                                            
         CLC   NBRSUPUT,APWORK+16                                               
         BNE   UPGR4                                                            
         CLC   NBRSUSHR,APWORK+17                                               
         BNE   UPGR4                                                            
         B     UPGR6                                                            
*                                                                               
UPGR2    XC    APELEM,APELEM       NO - BUILD UPGRADE ELEM                      
         LA    R4,APELEM                                                        
         MVI   NBRUPEL,NBRUPELQ                                                 
         MVI   NBRUPLEN,NBRUPLNQ                                                
*                                                                               
UPGR4    MVC   NBRUPFIL,APWORK                                                  
         MVC   NBRUPEXP,APWORK+1                                                
         MVC   NBRUPOBK,APWORK+9                                                
         TM    APWORK+9+1,BTY2CHAR                                              
         BNO   UPGR4G                                                           
         CLI   QBOOKTYP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   NBRUPOBT,QBOOKTYP                                                
         MVI   NBRUPLEN,NBRUPLQ2   EXTENDED LENGTH                              
UPGR4G   MVC   NBRUPINP(L'RESUPG),RESUPG                                        
         MVC   NBRSUPUT,APWORK+16                                               
         MVC   NBRSUSHR,APWORK+17                                               
         MVC   SVUPFILE,NBRUPFIL   SAVE UPGRADE VALUES                          
         MVC   SVUPGRD,NBRUPEXP                                                 
         MVC   SVUPFRBK,NBRUPOBK                                                
**       MVC   SVUPFRBK,NBRSBOOK                                                
         TM    NBRUPOBK+1,BTY2CHAR                                              
         BNO   UPGR5G                                                           
         CLI   NBRUPLEN,NBRUPLNQ                                                
         BH    *+6                                                              
         DC    H'0'                                                             
         CLI   NBRUPOBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SVUPFRBT,NBRUPOBT                                                
UPGR5G   XC    SVUPFBL,SVUPFBL                                                  
         CLI   NBRUPLEN,51                                                      
         BL    *+10                                                             
         MVC   SVUPFBL,NBRUPBKL                                                 
         MVC   SVUPINP,NBRUPINP                                                 
         MVC   SVUPPUT,NBRSUPUT                                                 
         MVC   SVUPSHR,NBRSUSHR                                                 
         OI    LCHG,LUPG                                                        
         OC    LAUPGEL,LAUPGEL                                                  
         BNZ   UPGR6                                                            
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         GOTO1 AADDELS,NBRRECD     ADD NEW UPGRADE ELEM                         
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
         MVC   LAUPGEL,16(R1)                                                   
*                                                                               
UPGR6    OC    APWORK+11(5),APWORK+11   TEST FOR OVERRIDE DAY/TIME              
         BNZ   UPGR8                                                            
         OC    LAODTEL,LAODTEL          NO - TEST FOR OVERRIDE ELEM             
         BZ    UPGR14                                                           
         MVI   APELEM,NBRODELQ               YES - DELETE IT                    
         GOTO1 ADELELS                                                          
         XC    LAODTEL,LAODTEL                                                  
         B     UPGR14                                                           
*                                                                               
UPGR8    ICM   R4,15,LAODTEL            YES - TEST FOR OVERRIDE ELEM            
         BZ    UPGR10                                                           
         USING NBRODEL,R4                                                       
         CLC   NBRODODY,APWORK+11            YES - TEST FOR CHANGES             
         BNE   UPGR12                                                           
         CLC   NBRODOTM,APWORK+12                                               
         BNE   UPGR12                                                           
         B     UPGR14                                                           
*                                                                               
UPGR10   XC    APELEM,APELEM       BUILD OVERRIDE ELEM                          
         LA    R4,APELEM                                                        
         MVI   NBRODEL,NBRODELQ                                                 
         MVI   NBRODLEN,NBRODLNQ                                                
*                                                                               
UPGR12   MVC   NBRODODY,APWORK+11                                               
         MVC   NBRODOTM,APWORK+12                                               
         OI    LCHG,LUPG                                                        
         OC    LAODTEL,LAODTEL                                                  
         BNZ   UPGR14                                                           
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         GOTO1 AADDELS,NBRRECD     ADD NEW OVERRIDE ELEM                        
         MVC   LAODTEL,16(R1)                                                   
*                                                                               
UPGR14   B     UPGRX2                                                           
*                                                                               
UPGRX1   MVC   FVMSGNO,=AL2(FVFOK)                                              
         CR    RA,RB                                                            
         B     EXIT                                                             
*                                                                               
UPGRX2   CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* INSPECT BOOK FIELDS                                                 *         
* OUTPUT : LCHG = LBK IF BOOKS WERE CHANGED                           *         
*          CC EQ  - OK                                                *         
*             NE  - ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
BOOKS    NTR1                                                                   
         TM    SVWKYIND,SVONEBK    TEST SINGLE BOOK, 4 WEEKS                    
         BO    BOOKX               YES-BOOKS ALREADY DETERMINED                 
         LA    R4,RESBK1H                                                       
         LA    R8,SVBKS                                                         
         LA    R3,SVBKTPS          SVBKS BOOKTYPES                              
         LA    R0,4                                                             
*                                                                               
BOOK2    TM    FVIIND-FVIHDR(R4),FVIVAL   VALIDATED?                            
         BO    BOOK6                - YUP YUP                                   
         LR    R1,R4               VALIDATE BOOK FIELD                          
         GOTO1 AFVAL                                                            
         BH    BOOKX                                                            
         BE    BOOK4                                                            
         OC    0(2,R8),0(R8)                                                    
         BZ    BOOK6                                                            
         XC    0(2,R8),0(R8)                                                    
         MVI   0(R3),0                                                          
         OI    LCHG,LBK                                                         
         B     BOOK6                                                            
*                                                                               
BOOK4    MVI   APWORK+16,C' '                                                   
         LA    R1,FVIFLD+5         TEST FOR '-WEEK' / TYPE                      
         CLI   0(R1),C'-'          TEST '-WEEK'                                 
         BE    BOOK4B                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'-'                                                       
         BE    BOOK4B              WE HAVE A WEEK                               
*                                                                               
         CLI   FVIFLD+3,C'/'       DO WE HAVE A SLASH?                          
         BE    BOOK4A1              - YEAH WE DO                                
         GOTO1 AGETBKTY,APPARM,(C'C',FVIFLD+5),0,APWORK+18 SPECIAL BT           
         CLI   0(R1),X'FF'                                                      
         BE    *+12                                                             
         LA    R1,FVIFLD+5                                                      
         B     BOOK4A3                                                          
         CLI   FVIFLD+5,C'+'       IGNORE LPM?                                  
         BE    BOOK4A3              - THAT'S THE CASE, LEAVE THE C'+'           
*                                                                               
BOOK4A1  GOTO1 AGETBKTY,APPARM,(C'C',FVIFLD+6),0,APWORK+18                      
         CLI   0(R1),X'FF'                                                      
         BNE   *+12                                                             
         CLI   FVIFLD+6,C'+'       IGNORING LPM?                                
         BNE   BOOK5                - WANT LPM, NOT WKLY OR SPECIAL BK          
****                                - DON'T WANT LPM, READ DEMO BOOK            
         LA    R1,FVIFLD+6                                                      
*                                                                               
BOOK4A3  MVC   APWORK+16(1),0(R1)  NOTE HAVE SPECIAL TYPE FOR THIS BOOK         
         MVI   0(R1),C' '                                                       
         CLI   1(R1),C' '                                                       
         BNH   BOOK4A5                                                          
         MVC   APWORK+17(1),1(R1)                                               
         MVI   1(R1),C' '                                                       
BOOK4A5  OC    SVWKY,SVWKY         WE DOING WEEKLY?                             
         BZ    BOOK5                - NOPE                                      
         B     BOOK4E               - YUP                                       
*                                                                               
BOOK4B   OC    SVWKY,SVWKY         YES-TEST WEEKLY OPTION SET                   
         BZ    BOOK9               NO-ERROR                                     
BOOK4E   CLC   1(1,R1),SVWKY+1     YES-TEST WEEK IS CORRECT                     
         BNE   BOOK9                                                            
         MVC   0(2,R1),SPACES                                                   
*                                                                               
BOOK5    LA    R2,FVIFLD           VALIDATE MONTH/YEAR                          
         ZIC   RF,FVILEN                                                        
         CLI   0(R2),C' '                                                       
         BH    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   RF,*-12                                                          
         B     BOOK9                                                            
         GOTO1 VDATVAL,APPARM,(2,(R2)),APWORK                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    BOOK9               INVALID DATE                                 
         GOTO1 VDATCON,APPARM,(0,APWORK),(3,APWORK+6)                           
*                                                                               
**  C'+' IS X'6D' WHICH IS HIGHER THAN C' ' AS WELL, CHECKED BOOK5G  **         
         CLI   APWORK+16,C' '      USER WANTS A SPECIAL BOOK TYPE?              
         BH    BOOK5G              YES, FORGET ABOUT LPM                        
         CLI   STABKTYP,C'H'       ONLY HISPANIC OR NO BOOK TYPE CAN            
         BE    BOOK5A                 HAVE PEOPLE METER DATA FOR NOW            
         CLI   STABKTYP,0                                                       
         BNE   BOOK5G                                                           
*                                                                               
BOOK5A   L     RE,ATWA                                                          
         AHI   RE,SVMLPMSD-TWAD                                                 
         OC    0(L'SVMLPMSD,RE),0(RE)  ANY LPM START DATE?                      
         BZ    BOOK5G              NONE                                         
         ST    RE,APPARM                                                        
         MVI   APPARM,2                                                         
         GOTO1 VDATCON,APPARM,,(3,APWORK+9)                                     
         CLC   APWORK+6(2),APWORK+9   BOOK ON OR AFTER LPM START DATE?          
         BL    BOOK5G                                                           
         OI    6(R4),X'80'            YES, USER WANT PEOPLE METER DATA          
*                                                                               
         CLI   STABKTYP,C'H'          HISPANIC PEOPLE METER DATA?               
         BNE   BOOK5D                                                           
         MVI   APWORK+16,C'I'             YES                                   
         MVI   8+6(R4),C'I'                                                     
         B     BOOK5G                                                           
*                                                                               
BOOK5D   MVI   APWORK+16,C'P'             NO, STANDARD PEOPLE METER             
         MVI   8+6(R4),C'P'                                                     
*                                  SET ANY BOOK OVERRIDE IN MONTH BYTE          
BOOK5G   CLI   APWORK+16,C'+'      IGNORING LPM?                                
         BE    BOOK5K               - YES, NO LPM PLEASE                        
*                                                                               
         GOTO1 AGETBKTY,APPARM,(C'C',APWORK+16),APWORK+6+1,APWORK+18            
*                                                                               
BOOK5K   CLC   0(2,R8),APWORK+6    COMPARE TO OLD VALUE                         
         BNE   BOOK5R                                                           
         CLC   0(1,R3),APWORK+18                                                
         BE    BOOK6                                                            
BOOK5R   MVC   0(2,R8),APWORK+6    SAVE BOOK YR/MN                              
         MVC   0(1,R3),APWORK+18   BINARY BOOKTYPE                              
         OI    LCHG,LBK                                                         
*                                                                               
BOOK6    CLI   APMODE,APMFRP                                                    
         BE   *+8                                                               
         OI    FVIIND-FVIHDR(R4),FVIVAL SET VALIDATED (ONLINE)                  
         SR    RF,RF               NEXT BOOK                                    
         ICM   RF,1,0(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,RF                                                            
         OC    0(2,R8),0(R8)       ANY BOOK?                                    
         BNZ   *+8                  - YUP                                       
         MVI   0(R3),0              - NOPE                                      
         LA    R8,2(R8)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,BOOK2            DO FOR ALL BOOKS                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     BOOKX                                                            
*                                                                               
BOOK9    MVC   FVMSGNO,=AL2(FVIBOOK)                                            
*                                                                               
BOOKX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS AND MESSAGES                                            *         
***********************************************************************         
         SPACE 1                                                                
ENOC     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
ENOUPG   MVC   FVMSGNO,=AL2(FVNOUPG)   WE NEED AN UPGRADE FORMULA!!             
         LA    R1,RESUPGH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
ESUB     MVC   FVMSGNO,=AL2(FVISDPT)                                            
         B     EXIT                                                             
ECMSEQ   MVC   FVMSGNO,=AL2(FVCMSEQ)                                            
         LA    R1,RESNUMH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
ENOSLN   MVC   FVMSGNO,=AL2(FVNOSLN)                                            
         B     EXIT                                                             
*                                                                               
EDEM     MVC   FVMSGNO,=AL2(FVIDEMOP)                                           
         LA    R1,BWSOPTH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
*                                                                               
EDUPREC  MVC   FVMSGNO,=AL2(FVDUPREC)                                           
         XC    BWSACT,BWSACT                                                    
         MVC   BWSACT(3),=C'DUP'                                                
         MVI   BWSACTH+FVILEN-FVIHDR,3                                          
         OI    BWSACTH+6,FVOXMT                                                 
         XC    BWSKEY,BWSKEY                                                    
         MVI   BWSKEY,C','                                                      
         MVI   BWSKEYH+FVILEN-FVIHDR,1                                          
         OI    BWSKEYH+6,FVOXMT                                                 
         LA    R1,BWSRECH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
AXTRA    DS    0F                  EXTENSION ROUTINE ADDRESSES                  
AVALRSP  DS    A                                                                
ADEMUP   DS    A                                                                
ASDEMUP  DS    A                                                                
AGETDEMS DS    A                                                                
AGETBKS  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DMREAD   DC    CL7'DMREAD '                                                     
DMWRITE  DC    CL7'DMWRT  '                                                     
CMT1     DC    CL10'ENTER=NEXT'                                                 
CMT1A    DC    CL8'PF1=COMP'                                                    
CMT2     DC    CL8'PF2=TRAN'                                                    
CMT3     DC    CL36'PF4=FRST PF5=PREV PF6=NEXT PF12=QUIT'                       
*                                                                               
QUESTION DC    CL8'????????'                                                    
SPACES   DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* INSPECT THE WEEKLY FIELD                                            *         
* OUTPUT : LCHG = LBK IF WEEKLY OPTION HAS CHANGED                    *         
*          CC EQ  - OK                                                *         
*             NE  - ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
WEEKLY   NTR1  BASE=*,LABEL=*                                                   
         XC    APHALF,APHALF                                                    
         NI    LFLAG,255-LGETBKS                                                
         NI    SVWKYIND,255-SVONEBK                                             
         GOTO1 AFVAL,RESWKYH       VALIDATE WEEKLY FIELD                        
         BH    WEEKX                                                            
         BL    WEEK6               MISSING                                      
         CLC   FVIFLD(2),=C'W='    TEST SINGLE WEEK NUMBER                      
         BNE   WEEK4                                                            
         CLI   FVILEN,3            YES-VALIDATE THE WEEK NUMBER                 
         BNE   WEEK9                                                            
         CLI   FVIFLD+2,C'1'                                                    
         BL    WEEK9                                                            
         CLI   FVIFLD+2,C'4'                                                    
         BH    WEEK9                                                            
         MVI   APHALF,C'W'                                                      
         MVC   APHALF+1(1),FVIFLD+2                                             
         OC    SVWKY,SVWKY         TEST WEEKLY ALREADY SET                      
         BZ    WEEK6                                                            
         CLI   SVWKY,C'W'          YES-TEST ONE BOOK, 4 WEEKS                   
         BE    WEEK6                                                            
         OI    LFLAG,LGETBKS       YES-GET BOOKS AGAIN                          
         B     WEEK6                                                            
*                                                                               
WEEK4    GOTO1 VSCANNER,APPARM,RESWKYH,(1,APELEM),C',=,-'                       
         CLI   4(R1),1                                                          
         BNE   WEEK9                                                            
         LA    R4,APELEM           VALIDATE MMM/YY-W FORM                       
         CLI   1(R4),1                                                          
         BNE   WEEK9                                                            
         CLI   22(R4),C'W'                                                      
         BNE   WEEK9                                                            
         GOTO1 VDATVAL,(R1),(2,12(R4)),APWORK                                   
         OC    0(4,R1),0(R1)                                                    
         BZ    WEEK9                                                            
         OI    SVWKYIND,SVONEBK    INDICATE SINGLE BOOK, 4 WEEKS                
         GOTO1 VDATCON,(R1),(0,APWORK),(3,APWORK+6)                             
         MVC   APHALF,APWORK+6                                                  
         LA    R0,4                ALL 4 BOOKS THE SAME                         
         LA    R1,SVBKS                                                         
         MVC   0(2,R1),APHALF                                                   
         LA    R1,2(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
WEEK6    CLC   APHALF,SVWKY        TEST WEEKLY OPTION CHANGED                   
         BE    WEEKX                                                            
         MVC   SVWKY,APHALF        YES-SAVE NEW WEEKLY OPTION                   
         OI    LCHG,LBK                AND INDICATE CHANGE                      
         OC    SVWKY,SVWKY         TEST WEEKLY OPTION SET                       
         BNZ   WEEKX                                                            
         OI    LFLAG,LGETBKS       NO-GET BOOKS AGAIN                           
         B     WEEKX                                                            
*                                                                               
WEEK9    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
WEEKX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE BOOKS                                                   *         
***********************************************************************         
DISBKS   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,4                DISPLAY THE BOOKS                            
         LA    R4,SVBKS                                                         
         LA    R3,SVBKTPS          SVBKS BOOK TYPES                             
         LA    R8,RESBK1H                                                       
         LA    R9,C'1'                                                          
         MVI   APWORK+2,1                                                       
*                                                                               
*****  IF UNABLE TO FIND LATEST BOOKS... WE PUT ???? INSTEAD OF YEAR 55         
DISB2    TM    LFLAG1,LF1NOLBK     DID WE FIND THE LATEST BOOK?                 
         BZ    DISB2A               - YUP WE DID, CONTINUE NORMALLY             
         MVC   8(8,R8),QUESTION    MOVE IN QUESTION MARKS ON SCREEN             
         XC    0(2,R4),0(R4)       CLEAR OUT SVBKS                              
         MVI   0(R3),0             CLEAR OUT SVBKTPS                            
         OI    FVIIND-FVIHDR(R8),FVIVAL   VALIDATED                             
         B     DISB4               NEXT, PLZ                                    
*****                                          MHC  02/05/03                    
*                                                                               
DISB2A   CLI   14(R8),C'+'         IGNORE LPM?                                  
         BE    *+10                 - YUP, DON'T CLEAR OUT BK ON SCREEN         
         XC    8(L'RESBK1,R8),8(R8)                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DISB4                                                            
*                                                                               
         MVC   APWORK(2),0(R4)                                                  
         NI    APWORK+1,X'FF'-BTYBITSQ   REMOVE SPECIAL TYPE BITS               
         GOTO1 VDATCON,APPARM,(3,APWORK),(6,8(R8))                              
*                                                                               
         TM    1(R4),BTYBITSQ      SPECIAL DATA?                                
         BNZ   DISB2X                 OTHER SPECIALS DON'T HAVE PPL MTR         
*                                                                               
         CLI   STABKTYP,C'H'       ONLY HISPANIC OR NO BOOK TYPE CAN            
         BE    DISB2D                 HAVE PEOPLE METER DATA FOR NOW            
         CLI   STABKTYP,0                                                       
         BNE   DISB3                                                            
*                                                                               
DISB2D   L     RE,ATWA             NO, SPECIAL TYPE SO LETS SEE IF LPM          
         AHI   RE,SVMLPMSD-TWAD                                                 
         OC    0(L'SVMLPMSD,RE),0(RE)  ANY LPM START DATE?                      
         BZ    DISB2G              NONE                                         
         ST    RE,APPARM                                                        
         MVI   APPARM,2            COMPRESSED DATE FORMAT                       
         GOTO1 VDATCON,APPARM,,(3,APWORK+8)                                     
*                                                                               
         CLC   APWORK(2),APWORK+8  ON OR AFTER LPM START DATE?                  
         BL    DISB2G              NOPE                                         
         CLI   14(R8),C'+'         IGNORING LPM?                                
         BE    DISB2G               - YES WE ARE                                
         CLI   STABKTYP,C'H'       STATION OR EST HAS HISPANIC BOOK?            
         BNE   *+12                                                             
         OI    1(R4),BTYHPEOQ      YES, SET ON PEOPLE METER                     
         B     DISB2X                                                           
*                                                                               
         OI    1(R4),BTYPEOPQ      YES, SET ON PEOPLE METER                     
         B     DISB2X                                                           
*                                                                               
DISB2G   OC    SVWKY,SVWKY         TEST WEEKLY OPTION                           
         BZ    DISB4                                                            
*                                                                               
DISB2X   MVI   14(R8),C'-'         YES-                                         
         TM    1(R4),BTYBITSQ      SPECIAL DATA?                                
         BZ    DISB3                                                            
         GOTO1 AGETBKTY,APPARM,(C'B',1(R4)),14(R8),0(R3)                        
*                                                                               
DISB3    OC    SVWKY,SVWKY         WEEKLY?                                      
         BZ    DISB4               NEITHER                                      
         STC   R9,15(R8)           1 BOOK, 4 WEEKS                              
         CLI   SVWKY,C'W'                                                       
         BNE   DISB4                                                            
         MVC   15(1,R8),SVWKY+1    4 BOOKS, 1 WEEK                              
*                                                                               
DISB4    OI    6(R8),FVOXMT                                                     
         LA    R4,2(R4)                                                         
         LA    R3,1(R3)            BUMPING THE SVBKTPS                          
         ZIC   RF,0(R8)                                                         
         AR    R8,RF                                                            
         LA    R9,1(R9)                                                         
         BCT   R0,DISB2                                                         
*                                                                               
DISBX    J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**B38X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALRSP                                                           
         B     DEMUP                                                            
         B     SDEMUP2                                                          
         B     GETDEMS                                                          
         B     GETBKS                                                           
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATING AND SHARE/PUT FIELDS                                *         
***********************************************************************         
         SPACE 1                                                                
VALRSP   LA    R8,RESRTGH          SCAN THE RATING AND SHARE/PUT FIELDS         
         MVI   LDEMNO,0                                                         
*                                                                               
VRSP1    CLI   LDEMNO,NDEMSCR      TEST SCANNED ALL FIELDS YET                  
         BNL   VRSPX               YES - DONE                                   
         ZIC   R9,TWADEM           DEMO DISPLACEMENT FOR FIRST ON SCRN          
         ZIC   R1,LDEMNO           RELATIVE DEMO ON SCREEN                      
         AR    R9,R1                                                            
         CLM   R9,1,SVDEMNO        COMPARE TO NUMBER OF ESTIMATE DEMOS          
         BNL   VRSPX               HI - DONE                                    
         LR    R1,R9                                                            
         MHI   R9,3                                                             
         LA    R9,LESTDEMS(R9)     R9 = A(CURRENT DEMO CODE)                    
         MHI   R1,12                                                            
         LA    R1,SVPROJ(R1)                                                    
         ST    R1,LAPROJ           LAPROJ = A(PROJECTED DEMO VALUES)            
         ICM   R1,15,LADEMEL       LOCATE DEMO IN BWS DETAIL RECORD             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DMOEL,R1                                                         
         ZIC   RF,DMOELLN                                                       
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    RE,L'DMODEMO                                                     
         LA    R4,DMODEMO                                                       
         CLC   1(2,R4),1(R9)                                                    
         BE    VRSP2               R4 = A(DEMO ENTRY IN DEMO ELEMENT)           
         BXLE  R4,RE,*-10                                                       
*                                                                               
         XC    APELEM,APELEM       NOT FOUND - ADD DEMO TO ELEMENT              
         ZIC   RF,DMOELLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)     SAVE THE ELEMENT                             
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         GOTO1 ADELELS,NBRRECD     DELETE IT FROM RECORD                        
         ZIC   RF,APELEM+1                                                      
         LA    RF,L'DMODEMO(RF)    LENGTHEN ELEMENT BY ONE DEMO                 
         STC   RF,APELEM+1                                                      
         GOTO1 AADDELS,NBRRECD     ADD BACK THE ELEMENT                         
         MVC   0(3,R4),0(R9)       MOVE IN THE DEMO CODE                        
         DROP  R1                                                               
*                                                                               
VRSP2    XC    APDUB,APDUB                                                      
         MVC   LSHR,0(R9)          SET THE SHARE AND PUT                        
         MVC   LPUT,0(R9)                                                       
         CLI   1(R9),C'I'                                                       
         BNE   *+16                                                             
         MVI   LSHR+1,C'X'                                                      
         MVI   LPUT+1,C'Q'                                                      
         B     *+12                                                             
         MVI   LSHR+1,C'S'                                                      
         MVI   LPUT+1,C'P'                                                      
*                                                                               
         ICM   R1,15,LASHPEL       TEST OVERRIDE SHARE/PUT ELEMENT              
         BZ    VRSP5               NO                                           
         USING NBRSHELD,R1         YES-SEARCH FOR OVERRIDE SHARE/PUT            
         ZIC   RF,NBRSHLEN                                                      
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    RE,L'NBRSHDMO                                                    
         LA    R1,NBRSHDMO                                                      
         DROP  R1                                                               
*                                                                               
VRSP3    TM    4(R1),NBRSHOVR      TEST REALLY IS AN OVERRIDE                   
         BZ    VRSP4                                                            
         CLC   1(2,R1),LSHR+1      TEST CORRECT DEMO CATEGORY                   
         BNE   *+12                                                             
         ST    R1,APDUB            APDUB=A(SHARE OVERRIDE)                      
         B     VRSP4                                                            
         CLC   1(2,R1),LPUT+1                                                   
         BNE   VRSP4                                                            
         ST    R1,APDUB+4          APDUB+4=A(PUT OVERRIDE)                      
VRSP4    BXLE  R1,RE,VRSP3                                                      
*                                                                               
VRSP5    XC    LDEMOLD,LDEMOLD     SAVE CURRENT DEMO VALUE                      
***      MVC   LDEMOLD+1(3),5(R4)                                               
         MVC   LDEMOLD,4(R4)       SAVE OFF THE 2D/OVERRIDE BIT AS WELL         
         TM    FVIIND-FVIHDR(R8),FVIVAL   TEST RATING FIELD CHANGED             
         BO    VRSP8                                                            
         OI    FVIIND-FVIHDR(R8),FVIVAL   YES-SET PREVIOUSLY VALIDATED          
         LR    R1,R8                                                            
         GOTO1 AFVAL               VALIDATE RATING FIELD                        
         BNE   VRSPX                                                            
         ZIC   RF,FVILEN                                                        
         ST    RF,APPARM+4                                                      
         LA    R1,FVIFLD           REMOVE THE * IF ANY                          
*                                                                               
VRSP6    CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                YES-REMOVE THE OVERRIDE                      
         LA    R1,1(R1)                                                         
         BCT   RF,VRSP6                                                         
         B     VRSP7                                                            
         MVC   LDEMLST(3),0(R4)    SINGLE UPGRADE FOR RATING                    
         MVI   LDEMLST+3,FF                                                     
         BAS   RE,SDEMUP                                                        
         MVC   4(4,R4),LDEMVAL                                                  
         L     RE,LAPROJ                                                        
         MVC   0(4,RE),LDEMVAL                                                  
         OI    LCHG,LRTG                                                        
         B     VRSP8                                                            
*                                                                               
VRSP7    DS    0H                                                               
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   IS 2 DECIMAL PRECISION ON?                   
         BZ    VRSP7E                                                           
         CLI   1(R4),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R4),C'E'                                                       
         BNE   VRSP7E              IT IS IMPRESSION                             
         GOTO1 VCASHVAL,APPARM,(2,FVIFLD)   2 DECIMAL CASHVAL CALL              
         B     VRSP7G                                                           
***  2 DECIMAL                                                                  
VRSP7E   GOTO1 VCASHVAL,APPARM,(1,FVIFLD)                                       
VRSP7G   CLI   APPARM,FF                                                        
         BE    VRSP99                                                           
         CLC   5(3,R4),APPARM+5    TEST FOR RATING CHANGE                       
         BNE   *+12                                                             
         TM    LFLAG,LTIMOVR+LDAYOVR   OR OVERRIDDEN DAYS/TIMES                 
         BZ    VRSP8                                                            
         MVC   4(4,R4),APPARM+4    YES - MOVE IN NEW RATING                     
         OI    4(R4),DMODEMOV            OVERRIDE INDICATOR                     
         OI    LCHG,LRTG                                                        
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   IS IT 2 DECIMALS?                            
         BNO   VRSP7H               - NOPE, NO NEED FOR FLAG                    
         CLI   1(R4),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R4),C'E'                                                       
         BNE   VRSP7H              IT IS IMPRESSION                             
         OI    4(R4),DMODEM2D      INDICATE 2 DECIMAL PRECISION                 
***  2 DECIMAL  ***                                                             
VRSP7H   L     RE,LAPROJ                 CHANGE THE SAVED RATING VALUE          
         MVC   0(4,RE),APPARM+4                                                 
         OI    0(RE),X'80'               INDICATE OVERRIDE                      
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   IS IT 2 DECIMALS?                            
         BNO   VRSP8                - NOPE, NO NEED FOR FLAG                    
         CLI   1(R4),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R4),C'E'                                                       
         BNE   VRSP8               IT IS IMPRESSION                             
         OI    0(RE),DMODEM2D      INDICATE 2 DECIMAL PRECISION                 
***  2 DECIMAL  ***                                                             
*                                                                               
VRSP8    LA    R8,RESSHPH-RESRTGH(R8)  SHARE/PUT FIELD                          
         LR    R1,R8                                                            
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL               VALIDATE SHARE/PUT FIELD                     
         BL    VRSPX                                                            
         BE    VRSP8A                                                           
         CLI   CLTBWPRO+15,C'Y'    MISSING-TEST IT'S OK                         
         BNE   VRSPX               NO                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VRSP18              YES                                          
*                                                                               
VRSP8A   CLI   CLTBWPRO+15,C'Y'    FOUND-TEST IT'S OK                           
         BE    VRSP99              NO                                           
         TM    FVIIND,FVIVAL       TEST FIELD CHANGED                           
         BO    VRSP18                                                           
         OI    FVIIND-FVIHDR(R8),FVIVAL   YES-SET FIELD PREV VALIDATED          
         XC    APELEM,APELEM                                                    
         LA    RF,C'/'                                                          
         STC   RF,APELEM(RF)                                                    
         TRT   FVIFLD(L'RESSHP),APELEM    SEARCH FOR / CHARACTER                
         BZ    VRSP99                                                           
         LR    R9,R1               R1 = A(C'/')                                 
         SR    RE,RE               LOCATE SHARE FIELD                           
         BCTR  RE,0                                                             
         LA    RF,FVIFLD                                                        
         BCTR  RF,0                                                             
         XC    APFULL,APFULL                                                    
*                                                                               
VRSP9    BXH   R9,RE,*+18                                                       
         OC    APFULL,APFULL                                                    
         BZ    VRSP99                                                           
         B     VRSP11                                                           
         CLI   0(R9),C' '                                                       
         BNH   VRSP10                                                           
         OC    APFULL,APFULL                                                    
         BNZ   VRSP9                                                            
         ST    R9,APFULL                                                        
         B     VRSP9                                                            
*                                                                               
VRSP10   OC    APFULL,APFULL                                                    
         BZ    VRSP9                                                            
*                                                                               
VRSP11   L     RF,APFULL                                                        
         SR    RF,R9                                                            
         ST    RF,APPARM+4                                                      
         LA    R9,1(R9)                                                         
         ST    R1,APFULL           APFULL = A(C'/')                             
         LR    R1,R9               REMOVE *, IF ANY                             
*                                                                               
VRSP11A  CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                YES-REMOVE THE OVERRIDE                      
         LA    R1,1(R1)                                                         
         BCT   RF,VRSP11A                                                       
         B     VRSP12                                                           
         MVC   LDEMLST(3),LSHR     REDO THE UPGRADE                             
         MVI   LDEMLST+3,FF                                                     
         BAS   RE,SDEMUP                                                        
         L     RE,LAPROJ                                                        
         MVC   4(4,RE),LDEMVAL     RAW DEMO VALUE                               
         OI    LCHG,LSHP                                                        
         ICM   RE,15,APDUB         TEST SHARE OVERRIDE ENTRY                    
         BZ    VRSP14                                                           
         NI    4(RE),255-NBRSHOVR  YES-OVERRIDE BIT OFF                         
         XC    APDUB(4),APDUB      REMOVE TRACE OF OVERRIDE                     
         B     VRSP14                                                           
*                                                                               
VRSP12   GOTO1 VCASHVAL,APPARM,(1,(R9))     VALIDATE SHARE FIELD                
         CLI   APPARM,FF                                                        
         BE    VRSP99                                                           
         OC    APPARM+4(4),APPARM+4  TEST SHARE=0                               
         BZ    VRSP99                YES-INVALID                                
         L     RE,LAPROJ                                                        
         CLC   5(3,RE),APPARM+5    TEST FOR SHARE CHANGE                        
         BE    VRSP14                                                           
         MVC   4(4,RE),APPARM+4    YES                                          
         OI    4(RE),X'80'         SHARE OVERRIDE                               
         ICM   R1,15,APDUB         TEST FOR ENTRY IN SHARE/PUT OVR ELEM         
         BNZ   VRSP13                                                           
         BAS   RE,ADDSHP           NO-ADD SHARE OVERRIDE ENTRY                  
         BNE   VRSPX                                                            
         L     R1,LASHPENT                                                      
         MVC   0(3,R1),LSHR                                                     
         ST    R1,APDUB                                                         
*                                                                               
VRSP13   MVC   4(4,R1),APPARM+4                                                 
         OI    4(R1),NBRSHOVR                                                   
         OI    LCHG,LSHP                                                        
*                                                                               
VRSP14   L     R9,APFULL           VALIDATE PUT FIELD                           
         LA    R9,1(R9)                                                         
         ZIC   RE,FVILEN                                                        
         LA    RE,FVIFLD(RE)                                                    
         SR    RE,R9                                                            
         ST    RE,APPARM+4                                                      
         LR    R1,R9               REMOVE *, IF ANY                             
*                                                                               
VRSP15   CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                YES-REMOVE OVERRIDE                          
         LA    R1,1(R1)                                                         
         BCT   RE,VRSP15                                                        
         B     VRSP16                                                           
         MVC   LDEMLST(3),LPUT     REDO THE UPGRADE                             
         MVI   LDEMLST+3,FF                                                     
         BAS   RE,SDEMUP                                                        
         L     RE,LAPROJ                                                        
         MVC   8(4,RE),LDEMVAL     RAW DEMO VALUE                               
         OI    LCHG,LSHP                                                        
         ICM   RE,15,APDUB+4       TEST PUT OVERRIDE ENTRY                      
         BZ    VRSP18                                                           
         NI    4(RE),255-NBRSHOVR  YES-OVERRIDE BIT OFF                         
         XC    APDUB+4(4),APDUB+4  REMOVE TRACE OF PUT OVERRIDE                 
         B     VRSP18                                                           
*                                                                               
VRSP16   GOTO1 VCASHVAL,APPARM,(1,(R9))                                         
         CLI   APPARM,FF                                                        
         BE    VRSP99                                                           
         OC    APPARM+4(4),APPARM+4    TEST PUT=0                               
         BZ    VRSP99                  YES-INVALID                              
         L     RE,LAPROJ                                                        
         CLC   9(3,RE),APPARM+5    TEST FOR PUT CHANGE                          
         BE    VRSP18                                                           
         MVC   8(4,RE),APPARM+4    YES-OVERRIDE                                 
         OI    8(RE),X'80'                                                      
         ICM   R1,15,APDUB+4       TEST FOR ENTRY IN SHARE/PUT OVR ELEM         
         BNZ   VRSP17                                                           
         BAS   RE,ADDSHP           NO-ADD PUT OVERRIDE ENTRY                    
         BNE   VRSPX                                                            
         L     R1,LASHPENT                                                      
         MVC   0(3,R1),LPUT                                                     
         ST    R1,APDUB+4                                                       
*                                                                               
VRSP17   MVC   4(4,R1),APPARM+4                                                 
         OI    4(R1),NBRSHOVR                                                   
         OI    LCHG,LSHP+LPUTOVR   INDICATE SHR/PUT CHANGE AND PUT OVR          
*                                                                               
VRSP18   TM    LCHG,LRTG+LSHP      TEST FOR ANY DEMO CHANGES                    
         BZ    VRSP28              NO-NEXT DEMO                                 
         L     R9,LAPROJ           R9=A(RATING/SHARE/PUT VALUES)                
         TM    0(R9),X'80'         TEST RATING OVERRIDE                         
         BO    VRSP19              YES                                          
         OC    APDUB,APDUB         NO-TEST ANY SHARE/PUT OVERRIDES              
         BZ    VRSP22              NO-THEN ADJUST THE SHARE                     
         SR    R1,R1               YES-CALCULATE NEW RATING                     
         L     R0,8(R9)                                                         
         SLL   R0,1                                                             
         SRDL  R0,32                                                            
         L     RE,4(R9)                                                         
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         MR    R0,RE                                                            
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   2 DECIMAL PRECISION?                         
         BZ    VRSP18E                                                          
         D     R0,=F'100'          ONLY DIVIDE BY 100                           
         B     VRSP18G                                                          
***  2 DECIMAL                                                                  
VRSP18E  D     R0,=F'1000'                                                      
VRSP18G  LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,0(R9)                                                         
         OI    0(R9),X'80'         RATING OVERRIDE BIT                          
         STCM  R1,15,4(R4)         STORE RATING IN BWS RECORD                   
         OI    4(R4),DMODEMOV      OVERRIDE BIT                                 
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   2 DECIMAL PRECISION?                         
         BZ    VRSP26               - NOPE                                      
         CLI   1(R4),C'R'          IS IT AN R                                   
         BE    *+12                                                             
         CLI   1(R4),C'E'          IS IT AN E                                   
         BNE   VRSP26                                                           
         OI    4(R4),DMODEM2D      2 DECIMAL BIT                                
         OI    0(R9),DMODEM2D                                                   
***  2 DECIMAL                                                                  
         B     VRSP26                                                           
*                                  THERE IS A RATING OVERRIDE --                
VRSP19   TM    LCHG,LSHP           TEST FOR SHARE/PUT CHANGE                    
         BZ    VRSP20              NO                                           
         TM    LCHG,LPUTOVR        YES-TEST PUT OVERRIDDEN THIS TIME            
         BO    VRSP22              YES-ADJUST THE SHARE                         
         TM    4(R9),X'80'         NO-TEST FOR SHARE OVERRIDE                   
         BO    VRSP24              YES-ADJUST THE PUT                           
         B     VRSP22              NO-ADJUST THE SHARE                          
*                                                                               
VRSP20   TM    8(R9),X'80'         TEST PUT OVERRIDE                            
         BO    VRSP22              YES-ADJUST THE SHARE                         
         TM    4(R9),X'80'         NO-TEST SHARE OVERRIDE                       
         BO    VRSP24              YES-ADJUST THE PUT                           
*                                                                               
VRSP22   OC    9(3,R9),9(R9)       ** ADJUST THE SHARE **                       
         BZ    VRSP26                                                           
         SR    R1,R1                                                            
         L     R0,0(R9)                                                         
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
***  2 DECIMAL  ***                                                             
         TM    0(R9),DMODEM2D      WE NEED 2 DECIMAL?                           
         BNO   *+12                                                             
         SLL   R0,1                NEED TO GET RID OF THE X'40' BIT             
         SRL   R0,1                WE'RE OK NOW                                 
***  2 DECIMAL  ***                                                             
         SRDL  R0,32                                                            
***  2 DECIMAL  ***                                                             
         TM    0(R9),DMODEM2D      2 DECIMAL VALUE?                             
         BNO   VRSP22G                                                          
         M     R0,=F'100'          ONLY NEED 100 INSTEAD OF 1000                
         B     VRSP22H                                                          
***  2 DECIMAL  ***                                                             
VRSP22G  M     R0,=F'1000'                                                      
VRSP22H  L     RE,8(R9)                                                         
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         STCM  R1,7,5(R9)                                                       
         ICM   RE,15,APDUB         TEST ENTRY IN SHARE/PUT OVERIDE ELEM         
         BZ    VRSP26                                                           
         TM    4(RE),NBRSHOVR      YES-TEST IT'S A REAL OVERRIDE                
         BZ    VRSP26                                                           
         STCM  R1,7,5(RE)          YES-UPDATE THE OVERRIDE                      
         B     VRSP26                                                           
*                                                                               
VRSP24   OC    5(3,R9),5(R9)       ** ADJUST THE PUT **                         
         BZ    VRSP26                                                           
         SR    R1,R1                                                            
         L     R0,0(R9)                                                         
         SLL   R0,1                ELIMINATE OVERRIDE BIT                       
***  2 DECIMAL  ***                                                             
         TM    0(R9),DMODEM2D      WE NEED 2 DECIMAL?                           
         BNO   *+12                                                             
         SLL   R0,1                NEED TO GET RID OF THE X'40' BIT             
         SRL   R0,1                WE'RE OK NOW                                 
***  2 DECIMAL  ***                                                             
         SRDL  R0,32                                                            
***  2 DECIMAL  ***                                                             
         TM    0(R9),DMODEM2D      2 DECIMAL VALUE?                             
         BNO   VRSP24G                                                          
         M     R0,=F'100'          ONLY NEED 100 INSTEAD OF 1000                
         B     VRSP24H                                                          
***  2 DECIMAL  ***                                                             
VRSP24G  M     R0,=F'1000'                                                      
VRSP24H  L     RE,4(R9)                                                         
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         STCM  R1,7,9(R9)                                                       
         ICM   RE,15,APDUB+4       TEST ENTRY IN SHARE/PUT OVERIDE ELEM         
         BZ    VRSP26                                                           
         TM    4(RE),NBRSHOVR      YES-TEST IT'S A REAL OVERRIDE                
         BZ    VRSP26                                                           
         STCM  R1,7,5(RE)          YES-UPDATE THE OVERRIDE                      
*                                                                               
VRSP26   OI    LCHG,LPROJ         INDICATE PROJECTED DEMO VALUES CHANGE         
         TM    INFIND,INFINOAD    TEST OPTION TO NOT AUTO ADJUST DEMOS          
         BO    VRSP28                                                           
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT   NO-TEST AUTO DEMO           
         BZ    VRSP28                                  ADJUSTMENTS              
**       OC    LDEMOLD,LDEMOLD     YES-TEST OLD DEMO VALUE = 0                  
**       BZ    VRSP98              YES-ERROR                                    
         BAS   RE,DEMADJ           NO-DO THE ADJUSTMENTS                        
         BNE   VRSP98              OLD DEMO VALUE = 0                           
*                                                                               
VRSP28   ZIC   RF,0(R8)            NEXT DEMO                                    
         AR    R8,RF                                                            
         LA    R8,RESRTGH-RESDM1H(R8)                                           
         IC    RF,LDEMNO                                                        
         LA    RF,1(RF)                                                         
         STC   RF,LDEMNO                                                        
         NI    LCHG,FF-LSHP-LRTG-LPUTOVR  RESET CHANGE INDICATOR                
         B     VRSP1                                                            
*                                                                               
VRSP98   MVC   FVMSGNO,=AL2(FVIDADJ)                                            
         B     VRSPX                                                            
*                                                                               
VRSP99   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VRSPX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A SHARE/PUT OVERRIDE                                 *         
* OUTPUT : LASHPENT=A(SHARE/PUT ENTRY IN SHARE/PUT ELEMENT)           *         
*          LASHPEL=A(SHARE/PUT ELEMENT) IF NEEDED TO ADD              *         
***********************************************************************         
         SPACE 1                                                                
ADDSHP   NTR1  ,                                                                
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         SR    R0,R0               LOCATE THE SHARE/PUT ELEMENT                 
         LA    RE,NBRFSTEL                                                      
ASHP1    CLI   0(RE),0                                                          
         BE    ASHP1A                                                           
         CLI   0(RE),NBRSHELQ                                                   
         BNE   *+8                                                              
         ST    RE,LASHPEL                                                       
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     ASHP1                                                            
*                                                                               
ASHP1A   XC    APELEM,APELEM                                                    
         ICM   RE,15,LASHPEL       TEST SHARE/PUT ELEMENT EXISTS                
         BZ    ASHP2               NO                                           
         ZIC   RF,1(RE)            YES-ADD SHARE/PUT TO THE ELEMENT             
         LR    R0,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(RE)                                                  
         GOTO1 ADELELS,NBRRECD                                                  
         LR    RF,R0                                                            
         LA    RF,L'NBRSHDMO(RF)                                                
         STC   RF,APELEM+1                                                      
         GOTO1 AADDELS,NBRRECD                                                  
         BNE   ASHPX                                                            
         L     R1,LASHPEL                                                       
         AR    R1,R0                                                            
         ST    R1,LASHPENT                                                      
         B     ASHPX                                                            
*                                                                               
ASHP2    LA    RE,APELEM                                                        
         USING NBRSHELD,RE                                                      
         MVI   NBRSHEL,NBRSHELQ                                                 
         LA    RF,NBRSHDMO-NBRSHEL+L'NBRSHDMO                                   
         STC   RF,NBRSHLEN                                                      
         GOTO1 AADDELS,NBRRECD                                                  
         BNE   VRSPX                                                            
         L     R1,ACPARM+16                                                     
         ST    R1,LASHPEL                                                       
         LA    R1,NBRSHDMO-NBRSHELD(R1)                                         
         ST    R1,LASHPENT                                                      
*                                                                               
ASHPX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R3,RE                                                            
         EJECT                                                                  
***********************************************************************         
* AUTO DEMO ADJUSTMENT ROUTINE                                        *         
* INPUT  : R4 = A(NEW DEMO ENTRY IN DEMO ELEMENT)                     *         
*          LADEMEL = A(DEMO ELEMENT)                                  *         
*          LDEMOLD = OLD RATING                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMADJ   NTR1                                                                   
         NI    LFLAG,FF-LADJALL                                                 
         OI    LFLAG,LADJIMP       ADJUST IMPRESSION                            
         TM    CMPOPTS,CAMOAALL+CAMOATGT  TEST AUTOADJ=ALL/TGT                  
         BZ    DEMA2                                                            
         MVC   APHALF,=X'D901'                                                  
         TM    CMPOPTS,CAMOAALL                                                 
         BO    *+10                                                             
         MVC   APHALF,ESTDEMS+1                                                 
         CLC   APHALF,1(R4)        YES-TEST TARGET IS ADJUSTMENT DEMO           
         BNE   DEMA2                                                            
         OI    LFLAG,LADJALL       YES                                          
         B     DEMA4                                                            
*                                                                               
DEMA2    CLI   1(R4),C'R'          TEST TARGET IS A RATING                      
         BE    DEMA4                                                            
         CLI   1(R4),C'E'          TEST TARGET IS A RATING                      
         BNE   DEMADJYS            NO - NO ADJUSTMENTS                          
*                                                                               
DEMA4    DS    0H                                                               
***  2 DECIMAL                                                                  
         XC    APWORK+64(16),APWORK+64                                          
         MVC   APWORK+64(4),LDEMOLD   OLD TARGET RATING                         
         MVC   APWORK+68(4),4(R4)   NEW TARGET RATING                           
*                                                                               
         NI    APWORK+64,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         NI    APWORK+68,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         OC    APWORK+64(4),APWORK+64   ANYTHING IN THE OLD RATING?             
         BZ    DEMADJNO                  - NOPE                                 
***                                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BZ    DEMA5                - NOPE WE'RE NOT                            
*                                                                               
         TM    LDEMOLD,X'40'       2 DECIMAL?                                   
         BNZ   DEMA4E               - YUP, NOTHING TO DO                        
         L     R1,APWORK+64                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+64                                                     
DEMA4E   TM    4(R4),X'40'         2 DECIMAL?                                   
         BNZ   DEMA5                                                            
         L     R1,APWORK+68                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+68                                                     
*****  BOTH APFULL AND LNEWRTG SHOULD BE IN 2 DECIMAL MODE BY NOW               
***  2 DECIMAL                                                                  
DEMA5    L     R1,APWORK+64        CALCULATE PCT ADJUSTMENT                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,APWORK+68+1                                                 
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,LDEMADJ          LDEMADJ = PCT ADJUSTMENT                     
*                                                                               
         L     R5,LADEMEL          SCAN ALL DEMOS IN DEMO ELEMENT               
         ZIC   R3,1(R5)                                                         
         AR    R3,R5                                                            
         BCTR  R3,0                                                             
         LA    R5,DMODEMO-DMOEL(R5)                                             
         LA    R2,L'DMODEMO                                                     
*                                                                               
DEMA6    CLC   1(2,R5),1(R4)       TEST OUR DEMO                                
         BE    DEMA14              YES - NO ADJ                                 
         TM    4(R5),DMODEMOV      TEST ALREADY MANUALLY OVERRIDDEN             
         BO    DEMA14              YES - NO ADJ                                 
         TM    LFLAG,LADJIMP       TEST TARGET IMPRESSION ADJUST                
         BZ    DEMA8                                                            
         CLI   1(R5),C'I'          YES - TEST ITS THE IMP WE WANT               
         BNE   DEMA8                                                            
         CLC   2(1,R5),2(R4)                                                    
         BE    DEMA10                    YES - ADJUST                           
*                                                                               
DEMA8    TM    LFLAG,LADJALL       TEST ADJUST ALL                              
         BZ    DEMA14              NO                                           
         CLI   1(R5),C'I'          YES - TEST ITS AN IMPRESSION                 
         BNE   DEMA10                    NO - GO AND ADJUST                     
         L     R8,LADEMEL                YES - CHECK THAT ITS RATING            
         LA    R8,DMODEMO-DMOEL(R8)            HAS NOT BEEN MANUALLY            
         CLI   1(R8),C'R'                      OVERRIDDEN                       
         BNE   *+14                                                             
         CLC   2(1,R8),2(R5)                                                    
         BE    *+12                                                             
         BXLE  R8,R2,*-18                                                       
         B     DEMA10                                                           
         TM    4(R8),DMODEMOV                                                   
         BO    DEMA14                                                           
*                                                                               
DEMA10   SR    R8,R8               DO THE ADJUSTMENT                            
         ICM   R8,7,5(R5)                                                       
         SR    R9,R9                                                            
         SRDA  R8,31                                                            
         M     R8,LDEMADJ                                                       
         D     R8,=F'1000'                                                      
         AHI   R9,1                                                             
         SRA   R9,1                                                             
***  2 DECIMAL                                                                  
         MVI   APBYTE,0                                                         
         TM    4(R5),X'40'         IS IT PREVIOUSLY 2 DECIMAL?                  
         BZ    DEMA11               - NOPE                                      
         OI    APBYTE,X'40'         - YUP, NEED THE 2 DECIMAL BIT ON            
***  2 DECIMAL                                                                  
DEMA11   ST    R9,4(R5)            STORE ADJUSTED DEMO IN ELEMENT               
         TM    APBYTE,X'40'                                                     
         BZ    DEMA11E                                                          
         OI    4(R5),X'40'                                                      
*                                                                               
DEMA11E  LA    RE,LESTDEMS         FIND OLD SAVED DEMO VALUES                   
         LA    R8,SVPROJ                                                        
*                                                                               
DEMA12   OC    0(3,RE),0(RE)                                                    
         BZ    DEMA14                                                           
         CLC   1(2,RE),1(R5)                                                    
         BE    *+16                                                             
         LA    RE,3(RE)                                                         
         LA    R8,12(R8)                                                        
         B     DEMA12                                                           
***  2 DECIMAL                                                                  
***      ST    R9,0(R8)            SAVE NEW ADJUSTED DEMO VALUE                 
         MVC   0(4,R8),4(R5)       SAVE NEW ADJUSTED DEMO VALUE                 
***  2 DECIMAL                                                                  
         TM    8(R8),X'80'         TEST PUT OVERRIDE                            
         BO    *+12                YES-ADJUST THE SHARE                         
         TM    4(R8),X'80'         NO-TEST SHARE OVERRIDE                       
         BO    DEMA13              YES-ADJUST THE PUT                           
         OC    9(3,R8),9(R8)                                                    
         BZ    DEMA14                                                           
         LR    R0,R9               CALCULATE NEW SHARE                          
         SR    R1,R1                                                            
         SLL   R0,1                                                             
***  2 DECIMAL  ***                                                             
         TM    0(R8),DMODEM2D      WE NEED 2 DECIMAL?                           
         BNO   *+12                                                             
         SLL   R0,1                NEED TO GET RID OF THE X'40' BIT             
         SRL   R0,1                WE'RE OK NOW                                 
***  2 DECIMAL  ***                                                             
         SRDA  R0,32                                                            
***  2 DECIMAL  ***                                                             
         TM    0(R8),DMODEM2D      2 DECIMAL VALUE?                             
         BNO   DEMA12G                                                          
         M     R0,=F'100'          ONLY NEED 100 INSTEAD OF 1000                
         B     DEMA12H                                                          
***  2 DECIMAL  ***                                                             
DEMA12G  M     R0,=F'1000'                                                      
DEMA12H  L     RE,8(R8)                                                         
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         STCM  R1,7,5(R8)          SAVE NEW SHR (PRESERVE OVERRIDE BIT)         
         ICM   RE,15,APDUB         TEST ENTRY IN SHARE/PUT OVERIDE ELEM         
         BZ    DEMA14                                                           
         TM    4(RE),NBRSHOVR      YES-TEST IT'S A REAL OVERRIDE                
         BZ    DEMA14                                                           
         STCM  R1,7,5(RE)          YES-UPDATE THE OVERRIDE                      
         B     DEMA14                                                           
*                                                                               
DEMA13   OC    5(3,R8),5(R8)                                                    
         BZ    DEMA14                                                           
         LR    R0,R9               CALCULATE NEW PUT                            
         SR    R1,R1                                                            
         SLL   R0,1                                                             
***  2 DECIMAL  ***                                                             
         TM    0(R8),DMODEM2D      WE NEED 2 DECIMAL?                           
         BNO   *+12                                                             
         SLL   R0,1                NEED TO GET RID OF THE X'40' BIT             
         SRL   R0,1                WE'RE OK NOW                                 
***  2 DECIMAL  ***                                                             
         SRDA  R0,32                                                            
***  2 DECIMAL  ***                                                             
         TM    0(R8),DMODEM2D      2 DECIMAL VALUE?                             
         BNO   DEMA13G                                                          
         M     R0,=F'100'          ONLY NEED 100 INSTEAD OF 1000                
         B     DEMA13H                                                          
***  2 DECIMAL  ***                                                             
DEMA13G  M     R0,=F'1000'                                                      
DEMA13H  L     RE,4(R8)                                                         
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         DR    R0,RE                                                            
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         STCM  R1,7,9(R8)          SAVE NEW PUT (PRESERVE OVERRIDE BIT)         
         ICM   RE,15,APDUB+4       TEST ENTRY IN SHARE/PUT OVERIDE ELEM         
         BZ    DEMA14                                                           
         TM    4(RE),NBRSHOVR      YES-TEST IT'S A REAL OVERRIDE                
         BZ    DEMA14                                                           
         STCM  R1,7,5(RE)          YES-UPDATE THE OVERRIDE                      
*                                                                               
DEMA14   BXLE  R5,R2,DEMA6         NEXT DEMO                                    
DEMADJYS CR    RE,RE               SET EQUAL CONDITION                          
         B     DEMAX                                                            
*                                                                               
DEMADJNO XR    R1,R1                                                            
         CR    RB,R1               SET != CONDITION                             
*                                                                               
DEMAX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SINGLE DEMO UPGRADE ROUTINE                                         *         
* INPUT  : LDEMLST=SINGLE DEMO                                        *         
* OUTPUT : LDEMVAL=DEMO VALUE                                         *         
***********************************************************************         
         SPACE 1                                                                
SDEMUP   NTR1                                                                   
*                                                                               
SDEMUP2  L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
         XC    LDEMVAL,LDEMVAL                                                  
         OC    SVUPGRD,SVUPGRD                                                  
         BZ    SDEMUPX                                                          
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         LA    RE,LIUN                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    SDEMUP2E             - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),QSTA+5     MOVE THE NETWORK IN                        
         MVC   SPUPSYSE,QSTA                                                    
         B     SDEMUP2G                                                         
*****  CABLE/FUSION DATE LOOKUP                                                 
SDEMUP2E MVC   SPUPSTA,QSTA                                                     
SDEMUP2G MVC   SPUPDAY,NBRSDAYS                                                 
         TM    LFLAG,LDAYOVR       TEST OVERRIDE DAYS                           
         BZ    *+10                                                             
         MVC   SPUPDAY,LDAYS                                                    
         MVC   SPUPTIM,NBRSTIMS                                                 
         TM    LFLAG,LTIMOVR       TEST OVERRIDE TIMES                          
         BZ    *+10                                                             
         MVC   SPUPTIM,LTIMES                                                   
         MVC   SPUPFIL,SVUPFILE                                                 
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,SVUPFRBK                                                 
         MVC   SPUPFBKL,SVUPFBL                                                 
         MVC   SPUPUDAY,SVUPDAY                                                 
         MVC   SPUPUTIM,SVUPTIM                                                 
         CLI   CUDMED,C'C'         TEST CANADA                                  
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         MVC   SPUPTYPE(L'SVUPGRD),SVUPGRD                                      
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         CLI   SPUPBTYP,0                                                       
         BNE   SDEMUP4                                                          
         CLI   CMPBKTYP,0                                                       
         BE    SDEMUP4                                                          
         MVC   SPUPBTYP,CMPBKTYP                                                
*                                                                               
SDEMUP4  DS    0H                                                               
         CLI   QBOOKTYP,0                                                       
         BNE   SDEMUP4E                                                         
         CLI   STABKTYP,0          ANYTHING HERE?                               
         BNE   SDEMUP4G             - NO, OVERRIDE CMPBKTYP REGARDLESS          
SDEMUP4E MVC   SPUPBTYP,QBOOKTYP                                                
SDEMUP4G TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    SDEMUP5                                                          
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPES?                       
         BNO   SDEMUP4R             - NOPE                                      
         CLI   SVUPFRBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,SVUPFRBT                                                
         B     SDEMUP4T                                                         
*                                                                               
SDEMUP4R GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
SDEMUP4T NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
SDEMUP5  CLI   SVUPPUT,C'1'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   SVUPPUT,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   SVUPSHR,C'1'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   SVUPSHR,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   SDEMUP8                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
SDEMUP8  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMLST,LDEMVAL                         
****  CANADA DOESN'T NEED TO OVERRIDE THE RATING SERVICE                        
         CLI   CUDMED,C'C'         TEST CANADA                                  
         BE    *+10                 - YUP, SKIP THE OVERRIDE                    
****  CANADA DOESN'T NEED TO OVERRIDE THE RATING SERVICE                        
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
*                                                                               
SDEMUPX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DEMO UPGRADE ROUTINE                                                *         
* OUTPUT : RATING SERVICE IS RETURNED IN LRTGSVC                      *         
***********************************************************************         
         SPACE 1                                                                
DEMUP    L     R3,AIOAREA3                                                      
         USING NBRRECD,R3                                                       
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         XC    APWORK,APWORK                                                    
         XC    APELEM,APELEM                                                    
         XC    LDEMOVR,LDEMOVR                                                  
         MVI   LRTGSVC,0                                                        
         NI    LFLAG,FF-LOVRALL                                                 
*                                                                               
         OC    SVUPGRD,SVUPGRD     TEST UPGRADE EXISTS                          
         BZ    DEMUX                                                            
         ICM   R8,15,LADEMEL       PREPARE FOR BXLE THRU DEMO ELEM              
         BZ    DEMU20                                                           
         USING DMOEL,R8                                                         
         ZIC   RF,1(R8)                                                         
         AR    RF,R8                                                            
         BCTR  RF,0                                                             
         LA    RE,L'DMODEMO                                                     
         LA    R1,DMODEMO                                                       
         USING DMODEMO,R1                                                       
*                                                                               
         TM    CMPOPTS,CAMOAALL+CAMOAIMP+CAMOATGT   TEST FOR AUTO DEMO          
         BZ    DEMU7                                ADJUSTS                     
         LA    R5,APELEM                                                        
*                                                                               
DEMU2    CLI   DMODEMO+1,C'R'      TEST RATING                                  
         BNE   DEMU5                                                            
         TM    DMODEMO+4,DMODEMOV  YES-TEST MANUAL OVERRIDE                     
         BZ    DEMU5                                                            
         TM    CMPOPTS,CAMOAALL+CAMOATGT  YES-TEST AUTOADJ=ALL/TGT              
         BZ    DEMU4                                                            
         MVI   APBYTE,1                                                         
         TM    CMPOPTS,CAMOAALL                                                 
         BO    *+10                                                             
         MVC   APBYTE,ESTDEMS+2                                                 
         CLC   APBYTE,DMODEMO+2    YES-TEST ADJUSTMENT DEMO                     
         BNE   DEMU4                                                            
         OI    LFLAG,LOVRALL       YES-INDICATE TO FREEZE ALL DEMOS             
         B     DEMU6                                                            
*                                                                               
DEMU4    MVI   0(R5),C'I'          FREEZE THE IMPRESSION                        
         MVC   1(1,R5),DMODEMO+2                                                
         LA    R5,2(R5)                                                         
*                                                                               
DEMU5    BXLE  R1,RE,DEMU2                                                      
*                                                                               
DEMU6    L     R1,LADEMEL                                                       
         LA    R1,DMODEMO-DMOEL(R1)                                             
*                                                                               
DEMU7    LA    R4,LDEMOVR          BUILD DEMO OVERRIDE LIST                     
         LA    R9,APWORK                                                        
*                                                                               
DEMU8    TM    DMODEMO+4,DMODEMOV  TEST MANUAL OVERRIDE                         
         BO    DEMU10                                                           
         TM    LFLAG,LOVRALL       NO-TEST ALL/TGT AUTO ADJUST ACTIVE           
         BO    DEMU10                                                           
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT  NO-TEST AUTO DEMO            
         BZ    DEMU11                                 ADJUST                    
         LA    R5,APELEM           YES - TEST FREEZE THIS DEMO                  
*                                                                               
DEMU9    CLI   0(R5),0                                                          
         BE    DEMU11                                                           
         CLC   0(2,R5),DMODEMO+1                                                
         BE    DEMU10              YES                                          
         LA    R5,2(R5)                                                         
         B     DEMU9                                                            
*                                                                               
DEMU10   MVI   0(R4),OVERELEM                                                   
         MVI   1(R4),6                                                          
         MVC   2(2,R4),DMODEMO+1                                                
         MVC   4(2,R4),DMODEMO+6                                                
         LA    R4,6(R4)                                                         
         TM    DMODEMO+4,DMODEMOV  TEST TRUE MANUAL OVERRIDE                    
         BZ    DEMU11                                                           
         MVC   0(2,R9),DMODEMO+1   YES - BUILD LIST IN APWORK                   
         LA    R9,2(R9)                                                         
*                                                                               
DEMU11   BXLE  R1,RE,DEMU8                                                      
         DROP  R1,R8                                                            
         ICM   R8,15,LASHPEL       TEST FOR SHARE/PUT OVERRIDE ELEM             
         BZ    DEMU20                                                           
         USING NBRSHELD,R8                                                      
         ZIC   RF,1(R8)            YES-                                         
         AR    RF,R8                                                            
         BCTR  RF,0                                                             
         LA    RE,L'NBRSHDMO                                                    
         LA    R1,NBRSHDMO                                                      
         USING NBRSHDMO,R1                                                      
*                                                                               
DEMU12   TM    NBRSHDMO+4,NBRSHOVR  TEST SHARE/PUT OVERRIDE                     
         BZ    DEMU19                                                           
         LA    R9,APWORK           YES-CHECK RATING/IMP IS OVERRIDDEN           
*                                                                               
DEMU13   CLI   0(R9),0                                                          
         BE    DEMU17                                                           
         CLI   0(R9),C'I'                                                       
         BNE   DEMU14                                                           
         CLI   NBRSHDMO+1,C'X'                                                  
         BE    DEMU15                                                           
         CLI   NBRSHDMO+1,C'Q'                                                  
         BE    DEMU15                                                           
         B     DEMU16                                                           
*                                                                               
DEMU14   CLI   NBRSHDMO+1,C'S'                                                  
         BE    DEMU15                                                           
         CLI   NBRSHDMO+1,C'P'                                                  
         BNE   DEMU16                                                           
*                                                                               
DEMU15   CLC   1(1,R9),NBRSHDMO+2                                               
         BE    DEMU18                                                           
*                                                                               
DEMU16   LA    R9,2(R9)                                                         
         B     DEMU13                                                           
*                                                                               
DEMU17   NI    NBRSHDMO+4,255-NBRSHOVR  NO-THEN REMOVE SHR/PUT OVERRIDE         
         OI    LCHG,LPRG                                                        
         B     DEMU19                                                           
*                                                                               
DEMU18   MVI   0(R4),OVERELEM      ADD TO OVERRIDE LIST                         
         MVI   1(R4),6                                                          
         MVC   2(2,R4),NBRSHDMO+1                                               
         MVC   4(2,R4),NBRSHDMO+6                                               
         LA    R4,6(R4)                                                         
*                                                                               
DEMU19   BXLE  R1,RE,DEMU12        NEXT SHARE/PUT                               
         DROP  R1,R8                                                            
*                                                                               
DEMU20   LA    R4,SVDEMS           REMOVE OVERRIDE INDICATORS                   
         CLI   0(R4),FF            FROM DEMO LIST                               
         BE    *+16                                                             
         MVI   0(R4),0                                                          
         LA    R4,3(R4)                                                         
         B     *-16                                                             
*                                                                               
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         LA    RE,LIUN                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    DEMU20E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),QSTA+5     MOVE THE NETWORK IN                        
         MVC   SPUPSYSE,QSTA                                                    
         B     DEMU20G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
DEMU20E  MVC   SPUPSTA,QSTA                                                     
DEMU20G  MVC   SPUPDAY,NBRSDAYS                                                 
         TM    LFLAG,LDAYOVR       TEST OVERRIDE DAYS                           
         BZ    *+10                                                             
         MVC   SPUPDAY,LDAYS                                                    
         MVC   SPUPTIM,NBRSTIMS                                                 
         TM    LFLAG,LTIMOVR       TEST OVERRIDE TIMES                          
         BZ    *+10                                                             
         MVC   SPUPTIM,LTIMES                                                   
         MVC   SPUPFIL,SVUPFILE                                                 
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,SVUPFRBK                                                 
         MVC   SPUPFBKL,SVUPFBL                                                 
         LA    R1,LDEMOVR                                                       
         ST    R1,SPUPAOVR                                                      
         MVC   SPUPUDAY,SVUPDAY                                                 
         MVC   SPUPUTIM,SVUPTIM                                                 
         CLI   CUDMED,C'C'         TEST CANADA                                  
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         MVC   SPUPTYPE(L'SVUPGRD),SVUPGRD                                      
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         CLI   SPUPBTYP,0                                                       
         BNE   DEMU20H                                                          
         CLI   CMPBKTYP,0                                                       
         BE    DEMU20H                                                          
         MVC   SPUPBTYP,CMPBKTYP                                                
*                                                                               
DEMU20H  DS    0H                                                               
         CLI   QBOOKTYP,0                                                       
         BNE   DEMU20K                                                          
         CLI   STABKTYP,0          ANYTHING HERE?                               
         BNE   DEMU20M              - NO, OVERRIDE CMPBKTYP REGARDLESS          
DEMU20K  MVC   SPUPBTYP,QBOOKTYP                                                
*                                                                               
DEMU20M  TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    DEMU20R                                                          
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPES?                       
         BNO   DEMU20P              - NOPE                                      
         CLI   SVUPFRBT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,SVUPFRBT                                                
         B     DEMU20R              - YUP, QBOOKTYP ALREADY THERE               
*                                                                               
DEMU20P  GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
DEMU20Q  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
DEMU20R  CLI   SVUPPUT,C'1'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   SVUPPUT,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   SVUPSHR,C'1'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   SVUPSHR,C'2'                                                     
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
         XC    SVPROJ(4*3*NDEMOS),SVPROJ                                        
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   DEMU20T                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
DEMU20T  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,SVDEMS,SVPROJ  CALL SPDEMUP             
*                                                                               
****  CANADA DOESN'T NEED TO OVERRIDE THE RATING SERVICE                        
         CLI   CUDMED,C'C'         TEST CANADA                                  
         BE    *+10                 - YUP, SKIP THE OVERRIDE                    
****  CANADA DOESN'T NEED TO OVERRIDE THE RATING SERVICE                        
         MVC   LRTGSVC,SPUPACTS    SAVE ACTUAL RATING SERVICE                   
         TM    LCHG,LUPG           TEST NEW UPGRADE                             
         BZ    DEMU21                                                           
         MVC   NBRSBOOK,SPUPFBK    YES-NEW ACTUAL SHARE BOOK                    
         TM    NBRSINDS,NBRSIPRG   MOVE NEW PROGRAM NAME IF NOT                 
         BO    DEMU21              ALREADY OVERRIDDEN                           
         MVI   NBRSPROG,C' '                                                    
         MVC   NBRSPROG+1(L'NBRSPROG-1),NBRSPROG                                
         MVC   NBRSPROG(L'SPUPPRG),SPUPPRG                                      
         LA    R0,L'NBRSPROG-2                                                  
         LA    R1,NBRSPROG+L'NBRSPROG-2                                         
         CLC   0(2,R1),=C'-S'                                                   
         BE    *+14                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-12                                                          
         B     DEMU21                                                           
         MVI   0(R1),C' '                                                       
         MVI   1(R1),C' '                                                       
*                                                                               
DEMU21   LA    R4,SVDEMS           SHOW OVERRIDES IN SVPROJ                     
         LA    R8,SVPROJ                                                        
*                                                                               
DEMU22   CLI   0(R4),FF                                                         
         BE    DEMU32                                                           
         LA    R1,LDEMOVR                                                       
*                                                                               
DEMU23   CLI   0(R1),0             SEE IF ANY OVERRIDES FOR THIS DEMO           
         BE    DEMU30              NO-THEN NO ADJUSTMENTS                       
         CLC   2(2,R1),1(R4)                                                    
         BE    DEMU24                                                           
         CLC   2(2,R1),4(R4)                                                    
         BE    DEMU24                                                           
         CLC   2(2,R1),7(R4)                                                    
         BE    DEMU24                                                           
         LA    R1,6(R1)                                                         
         B     DEMU23                                                           
*                                                                               
DEMU24   CLI   6(R4),OVERELEM      TEST PUT OVERRIDE                            
         BE    DEMU25              YES-ADJUST THE SHARE                         
         CLI   3(R4),OVERELEM      TEST SHARE OVERRIDE                          
         BE    DEMU26              YES-ADJUST THE PUT                           
         CLI   0(R4),OVERELEM      TEST RATING OVERRIDE                         
         BNE   DEMU30              NO-NO ADJUSTMENT NECESSARY                   
*                                                                               
DEMU25   OC    8(4,R8),8(R8)       ** ADJUST THE SHARE **                       
         BZ    DEMU27                                                           
         SR    R1,R1                                                            
         L     R0,0(R8)                                                         
***  2 DECIMAL  ***                                                             
         TM    0(R8),DMODEM2D      IS IT 2 DECIMAL?                             
         BNO   DEMU25E                                                          
         STCM  R0,8,APBYTE                                                      
         NI    APBYTE,FF-DMODEM2D   TURN OFF THE 2 DECIMAL X'40' BIT            
         ICM   R0,8,APBYTE                                                      
***  2 DECIMAL  ***                                                             
DEMU25E  SRDA  R0,31                                                            
***  2 DECIMAL  ***                                                             
         TM    0(R8),DMODEM2D      2 DECIMAL VALUE?                             
         BNO   DEMU25G                                                          
         M     R0,=F'100'          ONLY NEED 100 INSTEAD OF 1000                
         B     DEMU25H                                                          
***  2 DECIMAL  ***                                                             
DEMU25G  M     R0,=F'1000'                                                      
DEMU25H  D     R0,8(R8)                                                         
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         C     R1,4(R8)            TEST SHARE CHANGE                            
         BE    DEMU27              NO                                           
         ST    R1,4(R8)            YES-CHANGE THE SHARE                         
         CLI   3(R4),OVERELEM      TEST SHARE HAS BEEN OVERRIDDEN               
         BNE   DEMU27              NO                                           
         ICM   R1,15,LASHPEL       YES-FIND ORIGINAL SHARE OVERRIDE             
         BZ    DEMU27     <=== CHANGED TO BE AS IF NOT OVERRIDDEN               
****     DC    H'0'       <===    TO PREVENT DEATHS                             
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    RE,L'NBRSHDMO                                                    
         LA    R1,NBRSHDMO-NBRSHELD(R1)                                         
         USING NBRSHDMO,R1                                                      
         CLC   NBRSHDMO+1(2),4(R4)                                              
         BE    DEMU25J                                                          
         BXLE  R1,RE,*-10                                                       
         B     DEMU27     <=== CHANGED TO BE AS IF NOT OVERRIDDEN               
****     DC    H'0'       <===    TO PREVENT DEATHS                             
DEMU25J  MVC   NBRSHDMO+4(4),4(R8)                                              
         OI    NBRSHDMO+4,NBRSHOVR                                              
         OI    LCHG,LPRG           INDICATE CHANGE TO RECORD                    
         B     DEMU27                                                           
         DROP  R1                                                               
*                                                                               
DEMU26   OC    4(4,R8),4(R8)       ** ADJUST THE PUT **                         
         BZ    DEMU27                                                           
         SR    R1,R1                                                            
         L     R0,0(R8)                                                         
***  2 DECIMAL  ***                                                             
         TM    0(R8),DMODEM2D      IS IT 2 DECIMAL?                             
         BNO   DEMU26E                                                          
         STCM  R0,8,APBYTE                                                      
         NI    APBYTE,FF-DMODEM2D   TURN OFF THE 2 DECIMAL X'40' BIT            
         ICM   R0,8,APBYTE                                                      
***  2 DECIMAL  ***                                                             
DEMU26E  SRDA  R0,31                                                            
***  2 DECIMAL  ***                                                             
         TM    0(R8),DMODEM2D      2 DECIMAL VALUE?                             
         BNO   DEMU26G                                                          
         M     R0,=F'100'          ONLY NEED 100 INSTEAD OF 1000                
         B     DEMU26H                                                          
***  2 DECIMAL  ***                                                             
DEMU26G  M     R0,=F'1000'                                                      
DEMU26H  D     R0,4(R8)                                                         
         LA    R1,1(R1)                                                         
         SRA   R1,1                                                             
         ST    R1,8(R8)                                                         
*                                                                               
DEMU27   CLI   0(R4),OVERELEM      TEST FOR RATING OVERRIDE                     
         BNE   DEMU29              NO                                           
         LA    R9,APWORK           YES-SEE IF ITS A MANUAL OVERRIDE             
*                                                                               
DEMU28   CLI   0(R9),0                                                          
         BE    DEMU29                                                           
         CLC   0(2,R9),1(R4)                                                    
         BE    *+12                                                             
         LA    R9,2(R9)                                                         
         B     DEMU28                                                           
         OI    0(R8),X'80'         YES-RATING OVERRIDE BIT                      
*                                                                               
DEMU29   CLI   3(R4),OVERELEM      MARK SHARE/PUT OVERRIDES                     
         BNE   *+8                                                              
         OI    4(R8),X'80'                                                      
         CLI   6(R4),OVERELEM                                                   
         BNE   DEMU30                                                           
         OI    8(R8),X'80'                                                      
*                                                                               
DEMU30   LA    R4,9(R4)                                                         
         LA    R8,12(R8)                                                        
         B     DEMU22                                                           
*                                                                               
DEMU32   TM    LCHG,LUPG           TEST FOR UPGRADE CHANGE                      
         BZ    DEMU38                                                           
         LA    R2,SVPROJ           YES - CHANGE THE DEMO VALUES                 
         LA    R4,SVDEMS                 IN DETAIL DEMO ELEMENT                 
         ICM   R8,15,LADEMEL                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DMOEL,R8                                                         
*                                                                               
DEMU34   CLI   0(R4),FF                                                         
         BE    DEMU38                                                           
         CLI   0(R4),OVERELEM                                                   
         BE    DEMU36                                                           
         ZIC   RF,1(R8)                                                         
         AR    RF,R8                                                            
         BCTR  RF,0                                                             
         LA    RE,L'DMODEMO                                                     
         LA    R1,DMODEMO                                                       
         USING DMODEMO,R1                                                       
         CLC   1(2,R1),1(R4)                                                    
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DEMU36                                                           
         MVC   DMODEMO+4(4),0(R2)                                               
*                                                                               
DEMU36   LA    R2,12(R2)                                                        
         LA    R4,9(R4)                                                         
         B     DEMU34                                                           
*                                                                               
DEMU38   LA    R4,SVDEMS           REMOVE OVERRIDE INDICATORS                   
         CLI   0(R4),FF            FROM DEMO LIST                               
         BE    DEMUX                                                            
         MVI   0(R4),0                                                          
         LA    R4,3(R4)                                                         
         B     *-16                                                             
*                                                                               
DEMUX    B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* GET DEMO VALUES ROUTINE                                             *         
* ROUTINE ALSO EDITS THE PROGRAMS NAMES TO THE HEADLINES              *         
***********************************************************************         
         SPACE 1                                                                
         USING NBRSELD,R3                                                       
GETDEMS  XC    SPDEMLK,SPDEMLK                                                  
         LR    R1,R5                                                            
         AHI   R1,SVDEMXTN-TWAD                                                 
         ST    R1,SPLKXTND                                                      
         LA    RE,LIUN                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOM                                                    
         MVC   SPLKAGY,CUAALF                                                   
         MVC   SPLKMED,CUDMED                                                   
         MVC   SPLKCLI,QCLT                                                     
         MVI   SPLKFIL,C'T'                                                     
*****  CABLE/FUSION DATA LOOKUP                                                 
         XC    SPLKSTA,SPLKSTA                                                  
         USING SPLKXTD,RE                                                       
         L     RE,SPLKXTND         NEED TO PUT THE HEADEND IN EXTENDED          
         XC    SPXTAREA,SPXTAREA   CLEARING OUT THE EXTENDED AREA               
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    GETD00E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         MVC   SPLKSTA(3),QSTA+5   MOVE THE NETWORK IN                          
         MVC   SPXTHEAD,QSTA                                                    
         B     GETD00G                                                          
         DROP  RE                                                               
*****  CABLE/FUSION DATA LOOKUP         MHC  04/01/05                           
*                                                                               
GETD00E  MVC   SPLKSTA,QSTA                                                     
GETD00G  MVC   SPLKSRC,CLTSRC                                                   
         MVC   SPLKBTYP,STABKTYP                                                
*                                                                               
         CLI   SVRTGSVC,0          TEST SOURCE RETURNED FROM SPDEMUP            
         BE    GETD00K                                                          
         MVC   SPLKSRC,SVRTGSVC    YES-USE IT AND                               
****  CABLE/FUSION TEMP CODE                                                    
***      CLI   QSTA,C'0'                                                        
***      BL    *+10                NOT CABLE, SKIP IT                           
****  CABLE/FUSION TEMP CODE                                                    
***      B     *+10                    SUPPRESS MARKET OVERRIDE                 
GETD00K  MVC   SPLKUMK,MKTLKUP                                                  
         MVC   SPLKDAY,NBRSDAYS                                                 
         TM    LFLAG,LDAYOVR       TEST OVERRIDE DAYS                           
         BZ    *+10                                                             
         MVC   SPLKDAY,LDAYS                                                    
         MVC   SPLKTIM,NBRSTIMS                                                 
         TM    LFLAG,LTIMOVR       TEST OVERRIDE TIMES                          
         BZ    *+10                                                             
         MVC   SPLKTIM,LTIMES                                                   
         MVC   SPLKAVAL,AIOAREA1                                                
         MVI   SPLKSVI,FF                                                       
         CLI   SVWKY,C'W'          TEST 4 BOOKS, 1 WEEK                         
         BNE   *+14                                                             
         MVC   SPLKWKN,SVWKY+1     YES-SET WEEK NUMBER                          
         NI    SPLKWKN,X'0F'                                                    
         LA    R1,G1WPROF          NEED A(1W PROFILE FOR DEMOS)                 
         ST    R1,SPLKA1W                                                       
*        CLI   CUDMED,C'C'         TEST CANADA                                  
*        BNE   GETD1                                                            
*        TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
*        BZ    GETD1                                                            
*        MVI   3(R1),C'Y'                                                       
*                                                                               
GETD1    LA    R0,4                                                             
         LA    R1,SVDEMS                                                        
         ST    R1,SPLKALST                                                      
         LA    R2,1                                                             
         LA    R4,SVDEMVAL                                                      
         LA    R8,SVBKS                                                         
         LA    R1,SVBKTPS                                                       
         LA    RE,RESBK1H          NEED TO SEE IF C'+' IS THERE                 
         STCM  RE,15,APDUB                                                      
         XC    APWORK,APWORK                                                    
         LA    R9,APWORK                                                        
         USING HEDLINED,R9                                                      
*                                                                               
GETD2    MVC   SPLKBTYP,STABKTYP                                                
         ST    R1,LSAVER1          PRESERVE R1                                  
         L     RE,AIOAREA1         CLEAR OUTPUT AREA                            
****     LA    RF,4000                                                          
         LHI   RF,6000                                                          
         XCEF                                                                   
         XC    0(NDEMOS*3*4,R4),0(R4)                                           
         OC    0(2,R8),0(R8)                                                    
         BZ    GETD6                                                            
         MVC   SPLKDBK,0(R8)                                                    
         NI    SPLKOPT,X'FF'-SPLKOEXO                                           
*                                                                               
         TM    SPLKDBK+1,BTYBITSQ  SPECIAL BOOK INPUT BY USER??                 
         BZ    GETD2G              NO, NOT MANUALLY INPUT BY USER               
         TM    SPLKDBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   GETD2E               - NOPE                                      
         CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPLKBTYP,0(R1)       - YUP                                       
         B     GETD2F                                                           
*                                                                               
GETD2E   DS    0H                                                               
         GOTO1 AGETBKTY,APPARM,(C'B',SPLKDBK+1),SPLKBTYP                        
GETD2F   NI    SPLKDBK+1,X'FF'-BTYBITSQ                                         
         B     GETD3                                                            
*                                                                               
GETD2G   CLI   STABKTYP,C'H'       NO, HISPANIC OR NO BOOK TYPE CAN             
         BE    *+12                   HAVE PEOPLE METER DATA FOR NOW            
         CLI   STABKTYP,0                                                       
         BNE   GETD3                                                            
*                                                                               
         L     RE,ATWA             NO SPECIAL TYPE SO LETS SEE IF LPM           
         AHI   RE,SVMLPMSD-TWAD                                                 
         OC    0(L'SVMLPMSD,RE),0(RE)  ANY LPM START DATE?                      
         BZ    GETD3               NONE                                         
         ST    RE,APPARM                                                        
         MVI   APPARM,2            COMPRESSED DATE FORMAT                       
         GOTO1 VDATCON,APPARM,,(3,APFULL)                                       
*                                                                               
         CLC   SPLKDBK(2),APFULL   ON OR AFTER LPM START DATE?                  
         BL    GETSD2I             NO                                           
*                                                                               
         ICM   RE,15,APDUB         ADDRESS OF BOOK ON SCREEN                    
         CLI   14(RE),C'+'         IGNORE LPM?                                  
         BNE   GETD2K               - NOPE, WE NEED LPM                         
         B     GETD3                - YES, IGNORE LPM                           
*                                                                               
****   WE'RE BEFORE LPM START DATE!!!                                           
GETSD2I  ICM   RE,15,APDUB         ADDRESS OF BOOK ON SCREEN                    
         CLI   14(RE),C'+'         TRIED TO IGNORE LPM?                         
         BNE   *+12                 - NOPE, DIDN'T TRY, NO LPM ANYWAY           
         MVI   14(RE),C' '         CLEAR OUT THE C'+'                           
         OI    6(RE),X'80'         RETRANSMIT                                   
         B     GETD3                - YES, TRIED TO IGNORE LPM                  
****   WE'RE BEFORE LPM START DATE!!!                                           
*                                                                               
GETD2K   CLI   SPLKBTYP,C'H'       WAS ION OR EST HAS HISPANIC BOOK?            
         BNE   *+12                                                             
         MVI   SPLKBTYP,C'I'            SET ON HISPANIC PEOPLE METER            
         B     GETD3                                                            
         MVI   SPLKBTYP,C'P'            SET ON PEOPLE METER                     
*                                                                               
GETD3    CLI   SPLKBTYP,C'O'       OLYMPIC OVERRIDE?                            
         BNE   *+8                                                              
         OI    SPLKOPT,SPLKOEXO    YES                                          
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   GETD3C                                                           
         OI    SPLKOPT,SPLKOP2D    - YUP, SPECIAL 2 DECIMAL LOOKUP              
***  2 DECIMAL  ***                                                             
*                                                                               
GETD3C   TM    SVWKYIND,SVONEBK    TEST SINGLE BOOK, 4 WEEKS                    
         BZ    *+8                                                              
         STC   R2,SPLKWKN          YES-SET THE WEEK NUMBER                      
***  CABLE/FUSION                                                               
***      CLI   QSTA,C'0'                                                        
***      BL    GETD3G                                                           
***      CLI   SVRTGSVC,C'N'                                                    
***      BNE   GETD3G                                                           
***      MVI   SPLKBTYP,C'W'       WE NEED NSI WIRED                            
***  CABLE/FUSION                                                               
GETD3G   GOTO1 VSPDEMLK,APPARM,(X'FF',SPDEMLK)   CALL SPGETDEM                  
*                                                                               
         MVC   0(L'HEDPRG1,R9),SPLKPRG           PROGRAM NAME RETURNED          
         LR    R1,R4                                                            
         L     RE,AIOAREA1                                                      
         LA    RF,NDEMOS*3                                                      
*                                                                               
GETD4    MVC   0(4,R1),0(RE)       SAVE THE DEMO VALUES                         
         LA    R1,4(R1)                                                         
         LA    RE,8(RE)            EXCLUDE SVI VALUES                           
         BCT   RF,GETD4                                                         
*                                                                               
GETD6    LA    R4,NDEMOS*3*4(R4)                                                
         XR    RE,RE               LET'S CLEAR OUT RE                           
         ICM   RE,15,APDUB         MOVE TO THE NEXT BOOK ON SCREEN              
         XR    RF,RF               LET'S CLEAR OUT RF                           
         ICM   RF,1,0(RE)                                                       
         AR    RE,RF                                                            
         STCM  RE,15,APDUB                                                      
         LA    R8,2(R8)            NEXT BOOK                                    
         L     R1,LSAVER1          RESTORE R1                                   
         LA    R1,1(R1)            NEXT BOOK TYPE                               
         LA    R9,HEDPRG2-HEDPRG1(R9)                                           
         LA    R2,1(R2)                                                         
         BCT   R0,GETD2            DO FOR ALL BOOKS                             
*                                                                               
         LA    R9,RESHD1           MOVE PROGRAM NAMES TO HEADLINES              
         MVC   RESHD1,APWORK                                                    
         XC    HEDPRG1,HEDPRG1                                                  
         XC    HEDPRG3,HEDPRG3                                                  
         OI    RESHD1H+6,FVOXMT                                                 
         LA    R9,RESHD2                                                        
         MVC   RESHD2,APWORK                                                    
         XC    HEDPRG2,HEDPRG2                                                  
         XC    HEDPRG4,HEDPRG4                                                  
         OI    RESHD2H+6,FVOXMT                                                 
         MVC   RESPRG,NBRSPROG                                                  
         OI    RESPRGH+6,FVOXMT                                                 
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET THE BOOKS                                                       *         
***********************************************************************         
         SPACE 1                                                                
GETBKS   DS    0H                                                               
         OC    CMPBOOKS,CMPBOOKS   NONE - TEST FOR CAMPAIGN DEFINED BKS         
         BZ    GETBK00                                                          
         MVC   SVBKS,CMPBOOKS             YES - USE THOSE                       
         MVC   SVBKTPS,CMPBKTPS                                                 
         B     GETBX                                                            
*&&DO                                                                           
         XC    DBLOCK,DBLOCK       NO CAMPAIGN DEFINED BOOKS -                  
         MVC   DBBTYPE,QBOOKTYP    BOOKTYPE FROM THE MASTER RECORD              
         MVC   DBCOMFCS,ACOM       GET LATEST BOOK FROM DEMAND                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBAREC,AIOAREA1                                                  
         MVC   DBSELMED,CUDMED                                                  
         MVC   DBSELSRC,CLTSRC                                                  
         CLI   SVRTGSVC,0                                                       
         BE    *+10                                                             
         MVC   DBSELSRC,SVRTGSVC                                                
         MVC   DBSELUMK,BMKT                                                    
         MVC   DBSELSTA,QSTA                                                    
         MVC   DBSELAGY,CUAALF                                                  
******   MVC   DBSELDAT,=X'630C'   <=== TIM THOUGHT OF Y2K??                    
         MVI   DBFUNCT,DBGETTLB                                                 
         L     RF,ACOM                                                          
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),APPARM,DBLOCK,0                                             
*&&                                                                             
*                                                                               
GETBK00  XC    SPDEMLK,SPDEMLK                                                  
         MVC   SPLKBTYP,QBOOKTYP   BOOKTYPE FROM THE MASTER RECORD              
         MVC   SPLKAFAC,ACOM                                                    
         MVC   SPLKFIL,=C'TP '                                                  
         MVC   SPLKAREC,AIOAREA4                                                
         MVC   SPLKMED,CUDMED                                                   
         MVC   SPLKSRC,CLTSRC                                                   
         CLI   SVRTGSVC,0                                                       
         BE    *+10                                                             
         MVC   SPLKSRC,SVRTGSVC                                                 
         MVC   SPLKUMK,BMKT                                                     
         MVC   SPLKSPL,MKTRS                                                    
         MVC   SPLKSTA,QSTA                                                     
**  NEED TO SETUP THE EXTENDED AREA                                             
         LR    R1,R5                                                            
         AHI   R1,SVDEMXTN-TWAD                                                 
         ST    R1,SPLKXTND                                                      
**  NEED TO SETUP THE EXTENDED AREA                                             
         USING SPLKXTD,RE                                                       
         L     RE,SPLKXTND         NEED TO PUT THE HEADEND IN EXTENDED          
         XC    SPXTAREA,SPXTAREA   CLEARING OUT THE EXTENDED AREA               
         OI    SPXTFLAG,X'20'      DOING A DBGETTLB SPDEMLK                     
*****  CABLE/FUSION DATA LOOKUP                                                 
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    GETB1C               - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPLKSTA,SPLKSTA                                                  
         MVC   SPLKSTA(3),QSTA+5   MOVE THE NETWORK IN                          
         MVC   SPXTHEAD,QSTA                                                    
         B     GETB1E                                                           
         DROP  RE                                                               
*****  CABLE/FUSION DATA LOOKUP         MHC  04/01/05                           
GETB1C   MVC   SPLKSTA,QSTA                                                     
GETB1E   MVC   SPLKAGY,CUAALF                                                   
         LA    R1,APWORK                                                        
         ST    R1,SPLKAVAL         SPGETDEMF DIES WITHOUT THIS SET              
         GOTO1 VSPDEMLK,APPARM,(X'FF',SPDEMLK)   CALL SPGETDEM                  
*                                                                               
*        CLC   DBACTBK,=X'FFFF'    DID WE FIND LATEST BOOK?                     
*        BNE   GETB1G               - YUP, NO PROBLEM                           
         CLC   SPLKLBK,=X'FFFF'    DID WE FIND LATEST BOOK?                     
         BNE   GETB1G               - YUP, NO PROBLEM                           
         OI    LFLAG1,LF1NOLBK      - NOPE, TURN ON THE FLAG                    
*                                                                               
GETB1G   XC    SVBKS,SVBKS         CLEAR SAVED BOOKS FIELD                      
         LA    R4,SVBKS+6                                                       
         LA    R1,4                                                             
         MVC   0(2,R4),SPLKLBK                                                  
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    GETB12              NO - THEN INCLUDE LATEST BOOK                
*                                                                               
GETB2    MVC   0(2,R4),SPLKLBK     DETERMINE ALL FOUR BOOKS                     
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    *+14                                                             
         CLC   0(2,R4),SVUPFRBK    YES - COMPARE TO SHARE BOOK MONTH            
         BE    GETB12                                                           
         CLC   0(2,R4),SVUPGRD+2   COMPARE TO PUT MONTH                         
         BE    GETB12                                                           
         LA    R0,L'UMAJBKS                                                     
         LA    R8,UMAJBKS                                                       
         CLI   CUDMED,C'C'         TEST CANADIAN                                
         BNE   GETB3                                                            
         LA    R0,L'CMAJBKSA                                                    
         LA    R8,CMAJBKSA         BBM BOOKS                                    
         CLI   CLTSRC,C'N'                                                      
         BNE   GETB3                                                            
         LA    R0,L'CMAJBKSN                                                    
         LA    R8,CMAJBKSN         CSI BOOKS                                    
*                                                                               
GETB3    LR    RE,R8                                                            
         LR    R9,R0                                                            
*                                                                               
GETB4    CLC   1(1,R4),0(RE)                                                    
         BE    GETB12                                                           
         BL    GETB6                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,GETB4                                                         
*                                                                               
GETB6    CR    R0,R9                                                            
         BNE   GETB8                                                            
         LA    RF,0(R9,R8)                                                      
         BCTR  RF,0                                                             
         MVC   1(1,R4),0(RF)                                                    
         ZIC   RF,SPLKLBK                                                       
         BCTR  RF,0                                                             
         STC   RF,0(R4)                                                         
         B     GETB12                                                           
*                                                                               
GETB8    BCTR  RE,0                                                             
         SR    RF,RF                                                            
GETB10   IC    RF,1(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,1(R4)                                                         
         CLC   1(1,R4),0(RE)                                                    
         BNH   GETB12                                                           
         CLC   0(2,R4),SVUPFRBK                                                 
         BE    GETB12                                                           
         CLC   0(2,R4),SVUPGRD+2                                                
         BNE   GETB10                                                           
*                                                                               
GETB12   MVC   SPLKLBK,0(R4)                                                    
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         STC   RF,SPLKLBK+1                                                     
         B     GETB14                                                           
         MVI   SPLKLBK+1,12                                                     
         IC    RF,0(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,SPLKLBK                                                       
*                                                                               
GETB14   BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         BCT   R1,GETB2                                                         
*                                                                               
GETBX    B     XIT                                                              
         SPACE 2                                                                
UMAJBKS  DC    XL4'0205070B'       USA MAJOR BOOKS                              
CMAJBKSA DC    XL3'03070B'         CANADIAN BBM BOOKS                           
CMAJBKSN DC    XL4'0103080B'       CANADIAN CSI BOOKS                           
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
NDEMOS   EQU   14                                                               
NDEMSCR  EQU   6                                                                
FF       EQU   X'FF'                                                            
OVERELEM EQU   X'DE'                                                            
         EJECT                                                                  
LOCALD   DSECT                                                                  
LASAVE   DS    A                                                                
LAPROJ   DS    A                                                                
LANDX    DS    A                                                                
LACOST   DS    A                                                                
LAUPGEL  DS    A                                                                
LAODTEL  DS    A                                                                
LADEMEL  DS    A                                                                
LASHPEL  DS    A                                                                
LASHPENT DS    A                                                                
LDEMADJ  DS    F                                                                
LDEMOLD  DS    F                                                                
LSAVER1  DS    F                                                                
LDNAMES  DS    (NDEMOS)CL7                                                      
LDMUPBLK DS    (SPDEMUP2)X                                                      
LESTDEMS DS    XL((NDEMOS+1)*3)                                                 
LDEMNO   DS    X                                                                
LNDEMSCR DS    X                                                                
LRTGSVC  DS    C                                                                
LDEMOVR  DS    XL(6*3*NDEMOS+1)                                                 
LSVDPTLN DS    XL2                                                              
LDEMLST  DS    XL4                                                              
LDEMVAL  DS    XL4                                                              
LSHR     DS    XL3                                                              
LPUT     DS    XL3                                                              
LDPT     DS    CL1                                                              
LDAYS    DS    XL1                                                              
LTIMES   DS    XL4                                                              
L1WPROF  DS    XL16                                                             
*                                                                               
BBUYLINE DS    XL1                 BINARY LINE NUMBER                           
BSEQNUM  DS    XL1                 BINARY SEQ NUMBER                            
*                                                                               
LFLAG    DS    X                                                                
LADJIMP  EQU   X'80'                                                            
LADJALL  EQU   X'40'                                                            
LOVRALL  EQU   X'20'                                                            
LGETBKS  EQU   X'10'                                                            
LNEWDAYS EQU   X'08'                                                            
LDAYOVR  EQU   X'04'                                                            
LNEWTIME EQU   X'02'                                                            
LTIMOVR  EQU   X'01'                                                            
*                                                                               
LFLAG1   DS    X                                                                
LF1NOLBK EQU   X'60'               CAN'T FIND LATEST BOOK                       
*                                                                               
LCHG     DS    X                                                                
LCOST    EQU   X'80'                                                            
LUPG     EQU   X'40'                                                            
LBK      EQU   X'20'                                                            
LPRG     EQU   X'10'                                                            
LRTG     EQU   X'08'                                                            
LSHP     EQU   X'04'                                                            
LPROJ    EQU   X'02'                                                            
LPUTOVR  EQU   X'01'                                                            
*                                                                               
LINDS    DS    X                                                                
LNOTPOCM EQU   X'80'                                                            
LADDREC  EQU   X'40'                                                            
LRECDEL  EQU   X'20'                                                            
         EJECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
         EJECT                                                                  
LIUN     DS    2000X                                                            
*                                                                               
         ORG   LOCALD+4096                                                      
*                                                                               
LOCALX   EQU   *                                                                
         EJECT                                                                  
HEDLINED DSECT                                                                  
HEDPRG1  DS    CL12                PROGRAM NAME 1                               
         DS    CL1                                                              
HEDPRG2  DS    CL12                PROGRAM NAME 2                               
         DS    CL1                                                              
HEDPRG3  DS    CL12                PROGRAM NAME 3                               
         DS    CL1                                                              
HEDPRG4  DS    CL12                PROGRAM NAME 4                               
         SPACE 2                                                                
LINE1D   DSECT                                                                  
L1RTG1   DS    CL10                                                             
         DS    X                                                                
L1RTG2   DS    CL12                                                             
         DS    X                                                                
L1RTG3   DS    CL12                                                             
         DS    X                                                                
L1RTG4   DS    CL12                                                             
         SPACE  2                                                               
LINE2D   DSECT                                                                  
         DS    XL5                                                              
L2SHP1   DS    CL12                                                             
         DS    X                                                                
L2SHP2   DS    CL12                                                             
         DS    X                                                                
L2SHP3   DS    CL12                                                             
         DS    X                                                                
L2SHP4   DS    CL12                                                             
         EJECT                                                                  
SAVED    DSECT                     OVERLAY SAVE AREA DSECT                      
SVBKS    DS    XL8                                                              
SVBKTPS  DS    XL4                 SVBKS BOOKTYPES                              
SVWKY    DS    XL2                                                              
SVWKYIND DS    XL1                                                              
SVONEBK  EQU   X'80'                                                            
SVDEMNO  DS    X                                                                
SVUPFILE DS    C                                                                
SVUPGRD  DS    XL8                                                              
SVUPFRBK DS    XL2                                                              
SVUPFRBT DS    XL1                 SVUPFRBK BOOKTYPE                            
SVUPFBL  DS    XL6                                                              
SVUPINP  DS    CL32                                                             
SVUPDAY  DS    X                                                                
SVUPTIM  DS    XL4                                                              
SVUPPUT  DS    CL1                                                              
SVUPSHR  DS    CL1                                                              
SVRTGSVC DS    CL1                                                              
SVDEMS   DS    (NDEMOS*3)XL3                                                    
         DS    X                                                                
SVDEMVAL DS    (NDEMOS*4*3)XL4                                                  
SVPROJ   DS    (NDEMOS*3)XL4                                                    
*                                                                               
         ORG   SAVED+1280      *** RESERVED FOR BWS38 ***                       
         DS    0H                                                               
         ORG   SAVED+6144                                                       
SAVEX    EQU   *                                                                
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSE7D                                                       
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSBRV                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSBRV                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMLKXTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPDEMLKXTD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078SPNWS38   05/14/09'                                      
         END                                                                    
