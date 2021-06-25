*          DATA SET SPNWS34A   AT LEVEL 084 AS OF 07/17/02                      
*PHASE T20734A,*                                                                
         TITLE 'BWS34 - BWS - LEAD-IN LEAD-OUT AND ROTATIONAL ANALYSIS'         
T20734   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20734**,RA,RR=RE                                              
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
         AH    R6,=Y(SAVAREAX-SAVAREA)                                          
         AH    R6,=H'1024'                                                      
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
         GOTO1 AVALMED,LEDMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,LEDBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD                          
         BZ    VALK10                                                           
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   VALKX                                                            
*                                                                               
VALK10   GOTO1 AVALCAM,LEDNUMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALKX                                                            
         MVC   BWHKCAM,BCAM                                                     
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BE    *+16                                                             
         LA    R1,LEDNUMH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
         GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LTGTDEM,ESTDEMS     SET TARGET FROM ESTIMATE                     
         OC    INORTG,INORTG       EXCEPT IF OVERRIDDEN                         
         BZ    *+10                                                             
         MVC   LTGTDEM,INORTG                                                   
         MVI   LTGTDEM+3,X'FF'                                                  
         XC    DBLOCK,DBLOCK       GET DEMO NAME                                
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    R0,ESTUSRNM                                                      
         XC    LDNAME,LDNAME                                                    
         GOTO1 VDEMOCON,APPARM,(1,LTGTDEM),(2,LDNAME),(C'S',DBLOCK),   C        
               (R0)                                                             
*                                                                               
         GOTO1 AVALSTA,LEDSTAH     VALIDATE STATION                             
         BNE   VALKX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
*                                                                               
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
         LA    R2,LEDLINH          VALIDATE LINE NUMBER                         
         ST    R2,FVADDR           SO WE POINT TO CORRECT FIELD ON ERR          
         CLI   5(R2),0                                                          
         BNE   VALK24                                                           
         CLI   LEDSEQH+5,0                                                      
         BNE   VALK26                                                           
         LR    R1,R2               SET R1 TO A(FIELD HDR) FOR ENOC              
         B     ENOC                MISSING INPUT FILED                          
*                                                                               
VALK24   TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    EIIF                NO                                           
         CLI   APACTN,ACTADD       ADDING                                       
         BE    EIIF                NO GOOD, THEY SHOULD USE SEQ                 
*                                                                               
         ZIC   R1,LEDLINH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,LEDLIN(0)                                                  
         CVB   R1,APDUB                                                         
         CHI   R1,256                                                           
         BH    EIIF                                                             
         LTR   R1,R1                                                            
         BZ    EIIF                                                             
         STC   R1,BBUYLINE         SAVE BINARY LINE NUMBER                      
         B     VALK30                                                           
*                                                                               
VALK26   LA    R2,LEDSEQH                                                       
         CLI   5(R2),0                                                          
         BE    VALK30                                                           
*                                                                               
         CLI   LEDLINH+5,0                                                      
         BNE   EIIF                CAN'T HAVE BOTH LINE AND SEQ NUMBERS         
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    EIIF                NO                                           
         ZIC   R1,LEDSEQH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,LEDSEQ(0)                                                  
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
VALK30A  GOTO1 AIO,DIRHI                                                        
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
         LA    R1,4000                                                          
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
         NI    APINDS,X'FF'-APIOKDIS-APIOKCHA-APIOKDEL                 X        
               -APIOKRES-APIOKADD                                               
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
         GOTO1 AVALDAY,LEDDAYH     VALIDATE DAYS                                
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALTIM,LEDTIMH     VALIDATE TIMES                               
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALDPL,LEDDLNH     VALIDATE DAYPART/LENGTH                      
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
         CLI   CMPDPOPT,C'M'       TEST SUBDPT SCHEDULED UNDER MASTER           
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                    YES-ERROR                                
         CLI   BSLN,0              MAKE SURE SPOT LENGTH IS SPECIFIED           
         BE    ENOSLN                                                           
*                                                                               
         MVI   LFLAG,0                                                          
         MVI   LFLAG2,0                                                         
         XC    LADEMEL,LADEMEL                                                  
         XC    LAUPGEL,LAUPGEL                                                  
         XC    LAODTEL,LAODTEL                                                  
         SR    R0,R0               LOCATE ELEMENTS                              
         LR    R8,R3                                                            
*                                                                               
VALK52   CLI   0(R8),0                                                          
         BE    VALK54                                                           
         CLI   0(R8),NBRDMELQ                                                   
         BNE   *+8                                                              
         ST    R8,LADEMEL          A(DEMO ELEMENT)                              
         CLI   0(R8),NBRUPELQ                                                   
         BNE   *+8                                                              
         ST    R8,LAUPGEL          A(UPGRADE ELEMENT)                           
         CLI   0(R8),NBRODELQ                                                   
         BNE   *+8                                                              
         ST    R8,LAODTEL          A(OVERRIDE ELEMENT)                          
         IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     VALK52                                                           
*                                                                               
VALK54   B     VALKX                                                            
*                                                                               
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
         LA    R1,LEDMEDH                                                       
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
         SR    R0,R0                                                            
         LA    R4,LEDCSTH          TURN PREV VALIDATED BITS ON                  
*                                                                               
FSTR2    TM    FVATRB-FVIHDR(R4),FVAPROT                                        
         BO    *+8                                                              
         OI    FVIIND-FVIHDR(R4),FVIVAL                                         
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   FSTR2                                                            
*                                                                               
         XC    SVUPFILE,SVUPFILE   CLEAR SAVED VALUES                           
         XC    SVUPGRD,SVUPGRD                                                  
         XC    SVUPFRBK,SVUPFRBK                                                
         XC    SVUPFBL,SVUPFBL                                                  
         XC    SVUPINP,SVUPINP                                                  
         XC    SVUPDAY,SVUPDAY                                                  
         XC    SVUPTIM,SVUPTIM                                                  
         XC    SVUPPUT,SVUPPUT                                                  
         XC    SVUPSHR,SVUPSHR                                                  
         XC    SVBKS,SVBKS                                                      
         MVI   SVRTGSVC,0                                                       
         XC    SVDEM,SVDEM                                                      
         MVI   SVDEM+3,X'FF'                                                    
         MVI   SVFLAG,SVFIRST      FIRST TIME FLAG                              
         MVC   SVDAYS,BDAYS        SET DAYS/TIMES                               
         MVC   SVTIMES,BTIMES                                                   
         MVC   SVPCT,=H'50'        DEFAULT PERCENT FOR ROTN ANALYSIS            
         MVI   SVHH,1              START WITH 1ST HALF-HOUR                     
*                                                                               
         OC    CMPUP,CMPUP         TEST FOR CAMPAIGN UPGRADE                    
         BZ    FSTR4                                                            
         MVC   SVUPFILE,CMPUF                                                   
         MVC   SVUPGRD,CMPUP                                                    
         MVC   SVUPFRBK,CMPFB                                                   
         MVC   SVUPFBL,CMPFBLST                                                 
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
         XC    SVUPFBL,SVUPFBL                                                  
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
FSTR8    BAS   RE,COST             INSPECT THE COST FIELD                       
         BL    FSTRX                                                            
***      L     R3,AIOAREA2         EDIT THE COST                                
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         MVI   EBFLOAT,C'$'                                                     
         MVC   EBAIN,LACOST                                                     
         LA    R1,LEDCST                                                        
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'LEDCST                                                  
         MVI   EBDECS,2                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    LEDCSTH+6,FVOXMT                                                 
*                                                                               
         BAS   RE,VALUPGRD         INSPECT UPGRADE FIELD                        
         BL    FSTRX                                                            
         BE    *+14                                                             
         MVC   LEDUPG,SVUPINP      NONE - EDIT UPGRADE EXPRESSION               
         OI    LEDUPGH+6,FVOXMT                                                 
*                                                                               
         CLI   ACSVACT,ACTROT      INSPECT PERCENT FIELD FOR ROTN ANAL          
         BNE   *+12                                                             
         BAS   RE,PCT                                                           
         BNE   FSTRX                                                            
*                                                                               
         BAS   RE,DEMO             INSPECT DEMO FIELD                           
         BNE   FSTRX                                                            
         OC    MKTLKUP,MKTLKUP     TEST POSSIBILITY OF MARKET OVERRIDE          
         BZ    FSTR10                                                           
         MVC   LDEMLST,SVDEM       YES-CALL SPDEMUP TO GET RTG SERVICE          
*                                                                               
         MVI   QBOOKTYP,0                                                       
         TM    SVUPFRBK+1,X'80'    OLYMPIC BOOK?                                
         BZ    *+8                                                              
         MVI   QBOOKTYP,C'O'                                                    
*                                                                               
         GOTO1 ASDEMUP                                                          
*                                                                               
FSTR10   CLI   ACSVACT,ACTROT      SKIP LEAD-IN/LEAD-OUT FOR ROT ANAL           
         BE    FSTR13                                                           
         MVC   APBYTE,BDAYS                                                     
         MVC   APFULL,BTIMES                                                    
         MVC   APHALF(1),BDPT                                                   
         MVC   APHALF+1(1),BSLN                                                 
         MVC   SVLIDAYS,APBYTE     DISPLAY LEAD-IN DAYS                         
         MVC   LEDLID,LEDDAY                                                    
         OI    LEDLIDH+6,FVOXMT                                                 
*                                                                               
         BAS   RE,GETLTIM          GET DEFAULT LEAD-IN/LEAD-OUT TIMES           
         SR    R9,R9                                                            
         LA    R1,LEDLITH                                                       
         BAS   RE,LEADTM           INSPECT LEAD-IN TIMES FIELD                  
         BL    FSTRX                                                            
         BE    FSTR11                                                           
         MVC   SVLITIME,SVDFLITM   MISSING-USE THE DEFAULT                      
         GOTO1 VUNTIME,APPARM,SVLITIME,LEDLIT  DISPLAY TIMES                    
         OI    LEDLITH+6,FVOXMT                                                 
         LA    R9,1                NOTE LEAD-IN TIMES WERE MISSING              
*                                                                               
FSTR11   MVC   SVLODAYS,APBYTE     DISPLAY LEAD-OUT DAYS                        
         MVC   LEDLOD,LEDDAY                                                    
         OI    LEDLODH+6,FVOXMT                                                 
*                                                                               
         LA    R1,LEDLOTH                                                       
         BAS   RE,LEADTM           INSPECT LEAD-OUT TIMES FIELD                 
         BL    FSTRX                                                            
         BH    *+14                                                             
         MVC   SVLOTIME,BTIMES                                                  
         B     FSTR12                                                           
         MVC   SVLOTIME,SVDFLOTM   MISSING-USE THE DEFAULT                      
         GOTO1 VUNTIME,APPARM,SVLOTIME,LEDLOT  DISPLAY LEAD-OUT TIMES           
         OI    LEDLOTH+6,FVOXMT                                                 
*                                                                               
FSTR12   MVC   BDAYS,APBYTE                                                     
         MVC   BTIMES,APFULL                                                    
         MVC   BDPT,APHALF                                                      
         MVC   BSLN,APHALF+1                                                    
         BAS   RE,GETDTLST         GET DAYS/TIMES LIST FOR LOOKUPS              
*                                                                               
FSTR13   BAS   RE,BOOKS            INSPECT BOOK FIELDS                          
         BNE   FSTRX                                                            
         OC    SVBKS,SVBKS                                                      
         BNZ   FSTR14              BOOKS INPUT                                  
*                                                                               
         GOTO1 AGETBKS             GET THE BOOKS                                
*                                                                               
FSTR14   BAS   RE,DISBKS           DISPLAY THE BOOKS                            
*                                                                               
         LA    R1,LEDPRGH                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+12                                                             
         BAS   RE,GETHH            GET HALF HOURS FOR ROTATIONAL ANAL           
         LA    R1,ROTPPRH                                                       
         MVC   8(L'LEDPRG,R1),NBRSPROG  DISPLAY PROGRAMMING                     
         OI    6(R1),FVOXMT                                                     
*                                                                               
         GOTO1 ADEMUP              DO THE UPGRADES                              
*                                                                               
         GOTO1 AGETDEMS            GET THE DEMO VALUES                          
*                                                                               
         BAS   RE,MINMAX           GET MIN/MAX TOLERANCE VALUES                 
*                                                                               
         LA    R4,LEDCMTH                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R4,ROTCMTH                                                       
         OI    6(R4),FVOXMT        FORMAT BOTTOM LINE                           
         LA    R4,8(R4)                                                         
         XC    0(L'LEDCMT,R4),0(R4)                                             
         CLI   APRECNUM,RECSID                                                  
         BNE   *+14                                                             
         MVC   0(L'CMT1,R4),CMT1                                                
         LA    R4,L'CMT1+1(R4)                                                  
         TM    TWAMODE,TWAMLSM                                                  
         BZ    FSTRX                                                            
         MVC   0(L'CMT2,R4),CMT2                                                
*                                                                               
FSTRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
DISKEY   GOTO1 =A(DISKEYF),RR=APRELO                                            
         B     EXIT                                                             
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
         B     DISRX                                                            
*                                                                               
DISR2    MVI   LFLAG2,0                                                         
         TM    SVFLAG,SVFIRST      TEST FIRST TIME                              
         BO    DISR44              YES-DISPLAY                                  
         CLI   APPFKEY,PFK02       NO-TEST PF2                                  
         BNE   DISR3                                                            
         CLI   APRECNUM,RECSID     YES-TEST SID RECORD                          
         BE    DISR3                                                            
         MVI   APPFKEY,0           NO-IGNORE                                    
*                                                                               
DISR3    MVC   APHALF,BDPT         INSPECT DAYS AND TIMES FIELDS                
         MVC   APBYTE,BDAYS                                                     
         GOTO1 AVALDAY,LEDDAYH                                                  
         BNE   DISRX                                                            
         CLC   BDAYS,NBRSDAYS      TEST DAYS = THE RECORD'S DAYS                
         BE    *+8                                                              
         OI    LFLAG2,LDAYOVR      NO-DAYS OVERRIDE                             
         CLC   BDAYS,SVDAYS        TEST DAYS ALTERED THIS TIME                  
         BE    *+14                                                             
         OI    LFLAG2,LNEWDAYS     YES-                                         
         MVC   SVDAYS,BDAYS                                                     
         MVC   BDAYS,APBYTE        RESTORE RECORD'S DAYS                        
*                                                                               
         MVC   APFULL,BTIMES                                                    
         GOTO1 AVALTIM,LEDTIMH                                                  
         BNE   DISRX                                                            
         MVC   BDPT(2),APHALF                                                   
         CLC   BTIMES,NBRSTIMS     TEST TIMES = THE RECORD'S TIMES              
         BE    *+8                                                              
         OI    LFLAG2,LTIMOVR      NO-TIMES OVERRIDE                            
         CLC   BTIMES,SVTIMES      TEST TIMES ALTERED THIS TIME                 
         BE    DISR4                                                            
         OI    LFLAG2,LNEWTIME     YES-                                         
         MVC   SVTIMES,BTIMES                                                   
         CLI   ACSVACT,ACTLED                                                   
         BNE   DISR4                                                            
         BAS   RE,GETLTIM          GET DEFAULT LEAD-IN/LEAD-OUT TIMES           
*                                                                               
DISR4    MVC   BTIMES,APFULL       RESTORE RECORD'S TIMES                       
*                                                                               
         TM    LEDCSTH+FVIIND-FVIHDR,FVIVAL    TEST COST FIELD CHANGED          
         BO    DISR5                                                            
         OI    LEDCSTH+FVIIND-FVIHDR,FVIVAL    YES -                            
         BAS   RE,COST                         INSPECT COST FIELD               
         BL    DISRX                                                            
         BE    DISR5                                                            
         L     RE,LACOST                                                        
         OC    0(L'NBRSCST1,RE),0(RE)          COST FIELD BLANK                 
         BZ    DISR5                                                            
         XC    0(L'NBRSCST1,RE),0(RE)                                           
         OI    LCHG,LCOST                                                       
*                                                                               
DISR5    TM    LEDUPGH+FVIIND-FVIHDR,FVIVAL    TEST UPGRADE CHANGED             
         BO    DISR8                                                            
         OI    LEDUPGH+FVIIND-FVIHDR,FVIVAL                                     
         BAS   RE,VALUPGRD                                                      
         BL    DISRX                                                            
         BE    DISR8                                                            
         OC    LAUPGEL,LAUPGEL     UPGRADE REMOVED - TEST FOR UPGRD ELE         
         BZ    DISR6                                                            
         OI    LCHG,LUPG           YES - INDICATE UPGRADE CHANGE                
         MVC   SVUPFILE,CMPUF            USE CAMPAIGN UPGRADE VALUES            
         MVC   SVUPGRD,CMPUP                                                    
         MVC   SVUPFRBK,CMPFB                                                   
         MVC   SVUPFBL,CMPFBLST                                                 
         MVC   SVUPINP,CMPUPIN                                                  
         MVI   APELEM,UPGELCDQ           DELETE UPGRADE ELEMENT                 
         L     R1,AIOAREA3                                                      
         GOTO1 ADELELS                                                          
         XC    LAUPGEL,LAUPGEL                                                  
         MVI   APELEM,ODTELCDQ           DELETE OVERRIDE ELEMENT                
         GOTO1 ADELELS                                                          
         XC    LAODTEL,LAODTEL                                                  
*                                                                               
DISR6    MVC   LEDUPG,CMPUPIN            DISPLAY CAMPAIGN UPGRADE               
         OI    LEDUPGH+6,FVOXMT                                                 
*                                                                               
DISR8    CLI   ACSVACT,ACTROT      FOR ROTATION ANALYSIS,                       
         BNE   DISR9                                                            
         TM    ROTPCTH+FVIIND-FVIHDR,FVIVAL  TEST PERCENT FIELD CHANGED         
         BO    DISR9                                                            
         OI    ROTPCTH+FVIIND-FVIHDR,FVIVAL  YES-                               
         BAS   RE,PCT                        VALIDATE IT                        
         BNE   DISRX                                                            
*                                                                               
DISR9    LA    R1,LEDDEMH                      TEST DEMO FIELD CHANGED          
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R1,ROTDEMH                                                       
         TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BO    DISR10                                                           
         OI    FVIIND-FVIHDR(R1),FVIVAL        YES -                            
         BAS   RE,DEMO                         INSPECT DEMO FIELD               
         BL    DISRX                                                            
*                                                                               
DISR10   LA    R1,LEDPRGH          TEST FOR PROGRAMMING CHANGE                  
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R1,ROTPPRH                                                       
         TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BO    DISR14                                                           
         OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         GOTO1 AFVAL                                                            
         BH    DISRX                                                            
         CLI   FVIFLD,C'='         TEST PROGRAM ADJACENCY CODE                  
         BNE   DISR12                                                           
         GOTO1 AVALADJ,FVIFLD+1    YES - VALIDATE                               
         BNE   DISRX                                                            
         CLC   NBRSADJC,ADJCODE    VALID - TEST CHANGE                          
         BE    DISR14                                                           
         MVC   NBRSADJC,ADJCODE    YES - SET ADJACENCY CODE                     
         OI    LCHG,LPRG                 INDICATE RECORD CHANGE                 
         B     DISR14                                                           
*                                                                               
DISR12   CLC   NBRSPROG,FVIFLD     TEST PROGRAM CHANGE                          
         BE    DISR14                                                           
         MVC   NBRSPROG,FVIFLD                                                  
         OI    NBRSINDS,NBRSIPRG                                                
         OI    LCHG,LPRG                                                        
*                                                                               
DISR14   CLI   ACSVACT,ACTROT      TEST ROTATIONAL ANALYSIS                     
         BNE   DISR16                                                           
         TM    LFLAG2,LNEWDAYS+LNEWTIME   AND NEW DAYS OR TIMES                 
         BZ    DISR33                                                           
         MVI   SVHH,1              YES-START FROM 1ST HALF HOUR,                
         BAS   RE,GETHH                GET HALF HOUR LIST                       
         B     DISR33                                                           
*                                                                               
DISR16   MVC   APHALF,BDPT                                                      
         MVC   APFULL,BTIMES                                                    
         TM    LFLAG2,LNEWDAYS     TEST DAYS OVERRIDDEN THIS TIME               
         BZ    DISR20                                                           
         CLC   SVLIDAYS,SVDAYS     YES-COMPARE PREV VALUE TO DAYS               
         BE    *+14                                                             
         OI    LCHG,LLEAD          CHANGE                                       
         MVC   SVLIDAYS,SVDAYS                                                  
         MVC   LEDLID,LEDDAY                                                    
         OI    LEDLIDH+6,FVOXMT                                                 
*                                                                               
DISR20   LA    R1,LEDLITH                                                       
         TM    FVIIND-FVIHDR(R1),FVIVAL   TEST LEAD-IN TIMES CHANGED            
         BZ    DISR22              YES                                          
         TM    LFLAG2,LNEWTIME     NO-TEST TIMES OVERRIDDEN THIS TIME           
         BO    DISR23              YES-USE DEFAULT LEAD-IN TIMES                
         B     DISR24                                                           
*                                                                               
DISR22   OI    FVIIND-FVIHDR(R1),FVIVAL                                         
         BAS   RE,LEADTM           INSPECT LEAD-IN TIMES FIELD                  
         BL    DISRX                                                            
         BH    DISR23                                                           
         CLC   SVLITIME,BTIMES     TEST FOR CHANGE                              
         BE    DISR24                                                           
         OI    LCHG,LLEAD          YES                                          
         MVC   SVLITIME,BTIMES                                                  
         B     DISR24                                                           
*                                                                               
DISR23   MVC   SVLITIME,SVDFLITM   MISSING-USE THE DEFAULT                      
         XC    LEDLIT,LEDLIT                                                    
         GOTO1 VUNTIME,APPARM,SVLITIME,LEDLIT  DISPLAY TIMES                    
         OI    LEDLITH+6,FVOXMT                                                 
*                                                                               
DISR24   TM    LFLAG2,LNEWDAYS     TEST DAYS OVERRIDDEN THIS TIME               
         BZ    DISR28                                                           
         CLC   SVLODAYS,SVDAYS     YES-COMPARE PREV VALUE TO DAYS               
         BE    *+14                                                             
         OI    LCHG,LLEAD          CHANGE                                       
         MVC   SVLODAYS,SVDAYS                                                  
         MVC   LEDLOD,LEDDAY                                                    
         OI    LEDLODH+6,FVOXMT                                                 
*                                                                               
DISR28   LA    R1,LEDLOTH                                                       
         TM    FVIIND-FVIHDR(R1),FVIVAL   TEST LEAD-OUT TIMES CHANGED           
         BZ    DISR29              YES                                          
         TM    LFLAG2,LNEWTIME     NO-TEST TIMES OVERRIDDEN THIS TIME           
         BO    DISR30              YES-SET DEFAULT LEAD-OUT TIMES               
         B     DISR32                                                           
*                                                                               
DISR29   OI    FVIIND-FVIHDR(R1),FVIVAL   YES-                                  
         BAS   RE,LEADTM                                                        
         BL    FSTRX                                                            
         BH    DISR30                                                           
         CLC   SVLOTIME,BTIMES     TEST FOR CHANGE                              
         BE    DISR32                                                           
         OI    LCHG,LLEAD          YES                                          
         MVC   SVLOTIME,BTIMES                                                  
         B     DISR32                                                           
*                                                                               
DISR30   MVC   SVLOTIME,SVDFLOTM   MISSING-USE THE DEFAULT                      
         XC    LEDLOT,LEDLOT                                                    
         GOTO1 VUNTIME,APPARM,SVLOTIME,LEDLOT  DISPLAY LEAD-OUT TIMES           
         OI    LEDLOTH+6,FVOXMT                                                 
*                                                                               
DISR32   MVC   BDAYS,APBYTE                                                     
         MVC   BTIMES,APFULL                                                    
         MVC   BDPT(2),APHALF                                                   
         BAS   RE,GETDTLST         GET DAYS/TIMES LIST FOR LOOKUPS              
*                                                                               
DISR33   TM    LCHG,LUPG+LLEAD+LDEMO TEST UPGRADE, LI/LO OR DEMO CHANGE         
         BNZ   *+12                                                             
         TM    LFLAG2,LNEWTIME+LNEWDAYS OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR34                                                           
         GOTO1 ADEMUP              YES - DO THE UPGRADES                        
*                                                                               
DISR34   LA    R0,4                TEST ANY BOOK CHANGES                        
         LA    R1,LEDBK1H                                                       
         SR    RE,RE                                                            
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R1,ROTBK1H                                                       
         LR    RF,R1                                                            
         TM    FVIIND-FVIHDR(R1),FVIVAL                                         
         BZ    DISR36                                                           
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         BCT   R0,*-14                                                          
         B     DISR38                                                           
*                                                                               
DISR36   LA    R0,4                YES-                                         
         OI    FVIIND-FVIHDR(RF),FVIVAL                                         
         IC    RE,0(RF)                                                         
         AR    RF,RE                                                            
         BCT   R0,*-10                                                          
         BAS   RE,BOOKS            INSPECT THE BOOK FIELDS                      
         BNE   DISRX                                                            
         BAS   RE,DISBKS           MAKE SURE BOOKS ARE DISPLAYED                
*                                                                               
DISR38   TM    LCHG,LBK+LLEAD+LDEMO  TEST BOOK, LI/LO OR DEMO CHANGE            
         BNZ   *+12                                                             
         TM    LFLAG2,LNEWTIME+LNEWDAYS OR DAYS/TIMES CHANGED THIS TIME         
         BZ    DISR40                                                           
         GOTO1 AGETDEMS            YES-GET THE DEMO VALUES                      
*                                                                               
DISR40   DS    0H                                                               
         GOTO1 AVALRTG             VALIDATE RATING FIELD                        
         BNE   DISRX                                                            
*                                                                               
         CLI   ACSVACT,ACTROT      FOR ROTATIONAL ANALYSIS,                     
         BNE   DISR42                                                           
         TM    LCHG,LUPG+LBK+LDEMO+LPCT+LRTG TEST FOR CHANGES THAT WILL         
         BNZ   *+12                          AFFECT THE TOLERANCES              
         TM    LFLAG2,LNEWTIME+LNEWDAYS                                         
         BZ    DISR42                                                           
         BAS   RE,MINMAX           YES-GET MIN/MAX TOLERANCE VALUES             
*                                                                               
DISR42   CLI   APPFKEY,PFK02       TEST SID TRANSFER                            
         BE    DISR90              YES-TRANSFER                                 
*                                                                               
*                                  NO - TEST FOR CHANGES THAT'LL AFFECT         
         TM    LCHG,LCOST+LUPG+LBK+LLEAD+LRTG+LDEMO+LPCT     THE SCREEN         
         BNZ   DISR44                   YES- RE-DISPLAY CURRENT SCREEN          
         TM    LFLAG2,LNEWTIME+LNEWDAYS SAME FOR NEW DAYS/TIMES                 
         BNZ   DISR44                                                           
         CLI   ACSVACT,ACTROT      NO-FOR ROTATION ANALYSIS,                    
         BNE   DISR43                                                           
         ZIC   R1,SVHH                ADVANCE TO NEXT SCREEN                    
         LA    R1,4(R1)                                                         
         STC   R1,SVHH                                                          
         CH    R1,SVNUMHH             TEST ANY MORE HALF HOURS                  
         BNH   DISR44                                                           
*                                                                               
DISR43   MVI   APMODE,APMLRP       TELL CONTROLLER THIS IS LAST                 
         B     DISR92              SCREEN                                       
*                                                                               
DISR44   BAS   RE,DISSRC           DISPLAY RATING SOURCE                        
         XC    EBLOCK,EBLOCK       INITIALIZE EDITOR BLOCK                      
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         CLI   ACSVACT,ACTLED                                                   
         BNE   DISR45                                                           
         MVC   LEDLIP,SVLIPROG     DISPLAY LI/LO PROGRAMS                       
         OI    LEDLIPH+6,FVOXMT                                                 
         MVC   LEDLOP,SVLOPROG                                                  
         OI    LEDLOPH+6,FVOXMT                                                 
         B     DISR58                                                           
*                                                                               
DISR45   LA    R4,ROTH1PH          FOR ROTATIONAL ANALYSIS,                     
         LA    R8,ROTCMTH                                                       
         SR    RE,RE                                                            
*                                                                               
DISR46   IC    RE,0(R4)            CLEAR THE SCREEN                             
         SH    RE,=H'9'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,DRCLC                                                         
         BE    DISR48                                                           
         EX    RE,DROC                                                          
         BZ    DISR48                                                           
         EX    RE,DRXC                                                          
         OI    6(R4),FVOXMT                                                     
*                                                                               
DISR48   LA    R4,9(RE,R4)                                                      
         CR    R4,R8                                                            
         BL    DISR46                                                           
         BAS   RE,DISHH            DISPLAY THE HALF HOURS                       
         LH    RE,SVNUMHH          DETERMINE N'HALF-HOURS ON THIS               
         ZIC   RF,SVHH             SCREEN                                       
         SR    RE,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RE,1(RE)                                                         
         CH    RE,=H'4'                                                         
         BNH   *+8                                                              
         LH    RE,=H'4'                                                         
         STH   RE,SVNUMDT          SET N'DAYS/TIMES TO N'HALF/HOURS             
*                                                                               
         LA    R2,ROTRPRH          ROTATION PROGRAMS                            
         OI    ROTRPRH+6,FVOXMT                                                 
         LA    R1,ROTRPR                                                        
         USING ROTPRGLD,R1                                                      
         MVC   LPRG1,SVPROGS                                                    
         MVC   LPRG2,SVPROGS+10                                                 
         MVC   LPRG3,SVPROGS+20                                                 
         MVC   LPRG4,SVPROGS+30                                                 
         ZIC   RF,SVHH             HALF-HOUR PROGRAMS                           
         LR    R9,RF                                                            
         MH    R9,=H'40'                                                        
         LA    R9,SVPROGS(R9)                                                   
         MH    RF,=H'10'                                                        
         LA    RF,SVPRJPRG(RF)                                                  
         LA    R2,ROTH1PH                                                       
*                                                                               
DISR50   OI    6(R2),FVOXMT                                                     
         LA    R1,8(R2)                                                         
         MVC   LPRG1,0(R9)                                                      
         MVC   LPRG2,10(R9)                                                     
         MVC   LPRG3,20(R9)                                                     
         MVC   LPRG4,30(R9)                                                     
         MVC   LPRG5,0(RF)                                                      
         LA    R2,ROTH2P-ROTH1P(R2)                                             
         LA    R9,40(R9)                                                        
         LA    RF,10(RF)                                                        
         BCT   RE,DISR50                                                        
*                                                                               
         OI    ROTPPRH+6,FVOXMT    PROJECTED PROGRAM FROM BWS RECORD            
         MVC   ROTPPR,NBRSPROG                                                  
         LA    R4,SVDEMVAL         FORMAT THE ROTATION BOOK VALUES              
         LA    R0,4                                                             
         OI    ROTRBKH+6,FVOXMT                                                 
         LA    R1,ROTRBK                                                        
         USING ROTRTGLD,R1                                                      
         LA    R8,LRRTG1                                                        
*                                                                               
DISR52   ST    R8,EBAOUT                                                        
         MVI   EBLOUT,L'LRRTG1-1                                                
         ST    R4,EBAIN                                                         
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         ZIC   RE,EBLOUT                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R8),0(R8)                                                    
         OC    0(4,R4),0(R4)                                                    
         BZ    DISR54                                                           
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR54   LA    R4,4(R4)                                                         
         LA    R8,LRRTG2-LRRTG1(R8)                                             
         BCT   R0,DISR52                                                        
*                                                                               
         LA    R4,SVPROJ           FORMAT PROJECTED RATING                      
         OI    ROTRPJH+6,FVOXMT                                                 
         XC    ROTRPJ,ROTRPJ                                                    
         MVI   EBLOUT,L'ROTRPJ                                                  
         LA    R1,ROTRPJ                                                        
         ST    R1,EBAOUT                                                        
         LA    R1,APFULL                                                        
         ST    R1,EBAIN                                                         
         MVC   APFULL,SVPROJ                                                    
         NI    APFULL,255-X'80'                                                 
         OC    APFULL,APFULL                                                    
         BZ    DISR56                                                           
         MVI   EBFLOAT,0                                                        
         TM    SVPROJ,X'80'                                                     
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR56   OI    ROTRCPH+6,FVOXMT    FORMAT THE PROJECTED CPP                     
         XC    ROTRCP,ROTRCP                                                    
         MVI   EBLOUT,L'ROTRCP                                                  
         LA    R1,ROTRCP                                                        
         ST    R1,EBAOUT                                                        
         L     R1,LACOST                                                        
         ICM   RE,15,0(R1)                                                      
         BZ    DISR58                                                           
         OC    APFULL,APFULL                                                    
         BZ    DISR58                                                           
         SRDA  RE,32                                                            
         M     RE,=F'20'                                                        
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
                                                                                
DISR58   MVI   EBFLOAT,0           FORMAT THE BOOK VALUES FOR ALL               
         MVI   EBDECS,1            DAYS/TIMES                                   
         MVI   EBSCIN,0                                                         
         LA    R0,6                                                             
         LA    R4,SVDEMVAL                                                      
         LA    R2,LEDROTH                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   DISR60                                                           
         LH    R0,SVNUMDT                                                       
         LA    R2,ROTH1DH                                                       
         ZIC   R4,SVHH                                                          
         SLL   R4,4                                                             
         LA    R4,SVDEMVAL(R4)                                                  
*                                                                               
DISR60   LA    R9,4                4 BOOKS                                      
         OI    6(R2),FVOXMT                                                     
         LA    R8,LRTG1-LILOL1D+8(R2)                                           
         LA    R1,L'LRTG1                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   DISR62                                                           
         LA    R8,LRRTG1-ROTRTGLD+8(R2)                                         
         LA    R1,L'LRRTG1-1                                                    
*                                                                               
DISR62   ST    R8,EBAOUT                                                        
         ST    R4,EBAIN                                                         
         STC   R1,EBLOUT                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R8),0(R8)                                                    
         OC    0(4,R4),0(R4)                                                    
         BZ    DISR64                                                           
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISR64   CLI   ACSVACT,ACTLED                                                   
         BNE   *+16                                                             
         LA    R1,L'LRTG1                                                       
         LA    R8,LRTG2-LRTG1(R8)                                               
         B     DISR66                                                           
         LNR   R1,R9                FOR ROTATIONAL ANALYSIS, PUT A *            
         AH    R1,=H'4'             IF UNDER OR OVER TOLERANCE                  
         SLL   R1,3                                                             
         LA    R1,SVMINMAX(R1)                                                  
         L     RE,0(R4)                                                         
         MH    RE,=H'10'                                                        
         C     RE,0(R1)                                                         
         BNL   *+12                                                             
         MVI   L'LRRTG1-1(R8),C'-'                                              
         B     *+16                                                             
         C     RE,4(R1)                                                         
         BNH   *+8                                                              
         MVI   L'LRRTG1-1(R8),C'+'                                              
         LA    R8,LRRTG2-LRRTG1(R8)                                             
         LA    R1,L'LRRTG1-1                                                    
*                                                                               
DISR66   LA    R4,4(R4)                                                         
         BCT   R9,DISR62           DO FOR ALL BOOKS                             
*                                                                               
         LA    R1,3                BUMP TO NEXT DEMOS FIELD                     
         CLI   ACSVACT,ACTLED                                                   
         BNE   DISR68                                                           
         BCTR  R1,0                                                             
         CH    R0,=H'6'                                                         
         BNE   DISR68                                                           
         LA    R2,LEDLIQH                                                       
         B     DISR70                                                           
*                                                                               
DISR68   SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,*-6                                                           
*                                                                               
DISR70   BCT   R0,DISR60           DO FOR ALL DAYS/TIMES                        
*                                                                               
         LA    R4,SVPROJ           FORMAT PROJECTED VALUES                      
         CLI   ACSVACT,ACTLED                                                   
         BNE   DISR72                                                           
         OI    LEDRTGH+6,FVOXMT                                                 
         OI    LEDCPPH+6,FVOXMT                                                 
         LA    R2,LEDRTG                                                        
         LA    R9,6                                                             
         B     DISR74                                                           
*                                                                               
DISR72   LA    R2,ROTH1DH                                                       
         OI    6(R2),FVOXMT                                                     
         ZIC   RE,SVHH                                                          
         SLL   RE,2                                                             
         AR    R4,RE                                                            
         LH    R9,SVNUMDT                                                       
*                                                                               
DISR74   LA    R1,L'LEDRTG         FORMAT PROJECTED RATING                      
         LR    RE,R2                                                            
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+12                                                             
         LA    R1,L'LRRTG5                                                      
         LA    RE,LRRTG5-ROTRTGLD+8(R2)                                         
         STC   R1,EBLOUT                                                        
         ST    RE,EBAOUT                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    0(0,RE),0(RE)                                                    
         LA    RE,APFULL                                                        
         ST    RE,EBAIN                                                         
         MVC   APFULL,0(R4)                                                     
         NI    APFULL,255-X'80'                                                 
         OC    APFULL,APFULL                                                    
         BZ    DISR76                                                           
         MVI   EBFLOAT,0                                                        
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   EBFLOAT,C'*'                                                     
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         CLI   ACSVACT,ACTROT      FOR ROTATIONAL ANALYSIS,                     
         BNE   DISR76                                                           
         L     R1,0(R4)            PUT A * IF OUTSIDE TOLERANCE                 
         MH    R1,=H'10'                                                        
         L     RE,EBAOUT                                                        
         C     R1,SVPMINMX                                                      
         BNL   *+12                                                             
         MVI   L'LRRTG5(RE),C'-'                                                
         B     DISR76                                                           
         C     R1,SVPMINMX+4                                                    
         BNH   DISR76                                                           
         MVI   L'LRRTG5(RE),C'+'                                                
*                                                                               
DISR76   LA    R1,L'LEDCPP         FORMAT THE CPP/CPM                           
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+16                                                             
         LA    R1,L'LRCPP                                                       
         LA    RE,LRCPP-ROTRTGLD+8(R2)                                          
         B     DISR78                                                           
         LA    RE,LEDCPP                                                        
         CH    R9,=H'6'                                                         
         BE    DISR78                                                           
         LA    RE,LCPP-LPROJECT(R2)                                             
*                                                                               
DISR78   ST    RE,EBAOUT                                                        
         STC   R1,EBLOUT                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    0(0,RE),0(RE)                                                    
         L     R1,LACOST                                                        
         ICM   RE,15,0(R1)                                                      
         BZ    DISR80                                                           
         OC    APFULL,APFULL                                                    
         BZ    DISR80                                                           
         SRDA  RE,32                                                            
         M     RE,=F'20'                                                        
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
DISR80   BCT   R9,*+8              DO FOR ALL DAYS/TIMES                        
         B     DISR88                                                           
         LA    R0,3                                                             
         CLI   ACSVACT,ACTLED                                                   
         BNE   DISR82                                                           
         BCTR  R0,0                                                             
         LR    R2,R8                                                            
         CH    R9,=H'5'                                                         
         BNE   DISR82                                                           
         LA    R8,LEDLIQH                                                       
         B     DISR84                                                           
*                                                                               
DISR82   SR    RE,RE                                                            
         IC    RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R0,*-6                                                           
         LR    R8,R2                                                            
         CLI   ACSVACT,ACTROT                                                   
         BNE   DISR84                                                           
         MVI   APBYTE,1                                                         
         B     DISR86                                                           
*                                                                               
DISR84   LA    R2,LPROJECT-LILOL1D+8(R8)                                        
*                                                                               
DISR86   OI    6(R8),FVOXMT                                                     
         LA    R4,4(R4)                                                         
         B     DISR74                                                           
*                                                                               
DISR88   CLI   ACSVACT,ACTLED                                                   
         BNE   DISR90                                                           
         OI    LEDIPRH+6,FVOXMT    DISPLAY LEAD-IN AND LEAD-OUT                 
         XC    LEDIPR,LEDIPR       PROGRAMS FOR ALL 4 BOOKS                     
         LA    R2,LEDIPR                                                        
         USING LILOL2D,R2                                                       
         LA    R8,LPRGBK1                                                       
         LA    R4,SVLIPRGS                                                      
         LA    R0,4                                                             
         MVC   0(L'LPRGBK1,R8),0(R4)                                            
         LA    R4,16(R4)                                                        
         LA    R8,LPRGBK2-LPRGBK1(R8)                                           
         BCT   R0,*-14                                                          
         MVC   LPRGPROJ,SVLIPROG   PROJECTED LEAD-IN PROGRAM                    
*                                                                               
         OI    LEDOPRH+6,FVOXMT    LEAD-OUT -                                   
         XC    LEDOPR,LEDOPR                                                    
         LA    R2,LEDOPR                                                        
         USING LILOL2D,R2                                                       
         LA    R8,LPRGBK1                                                       
         LA    R4,SVLOPRGS                                                      
         LA    R0,4                                                             
         MVC   0(L'LPRGBK1,R8),0(R4)                                            
         LA    R4,16(R4)                                                        
         LA    R8,LPRGBK2-LPRGBK1(R8)                                           
         BCT   R0,*-14                                                          
         MVC   LPRGPROJ,SVLOPROG   PROJECTED LEAD-OUT PROGRAM                   
*                                                                               
DISR90   CLI   APRECNUM,RECSID     TEST SID RECORD                              
         BNE   DISR92                                                           
         L     R0,LASAVE           YES - SAVE BWS RECORD THAT'S BEEN            
         AH    R0,=Y(SVREC-SAVAREA)      BUILT FROM IT                          
         LA    R1,4000                                                          
         L     RE,AIOAREA2                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   APPFKEY,PFK02       TEST SID TRANSFER                            
         BNE   DISRX                                                            
         MVI   APPFKEY,0                                                        
         GOTO1 AXFRADD             YES - TRANSFER SID TO BWS                    
         BNE   DISRX                                                            
         MVI   SCPFKEY,PFK05             AND RETURN TO LIST SCREEN              
         MVI   APMODE,APMLRP                                                    
         B     DISRX                                                            
*                                                                               
DISR92   TM    SVFLAG,SVFIRST      TEST NOT FIRST TIME                          
         BO    DISRX                                                            
         TM    LCHG,LCOST+LUPG+LPRG+LRTG    AND ANY DETAIL RECORD               
         BZ    DISRX                        CHANGES                             
         L     R1,AIOAREA3                                                      
         MVC   IOKEY(13),0(R1)                                                  
*                                                                               
         GOTO1 AIO,DIRHIU+IO2                                                   
         BE    DISR92B                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
DISR92B  CLC   IOKEY(13),IOKEYSAV  CAN WE ADD THIS RECORD?                      
         BNE   DISR95              YES                                          
         TM    IOERR,IOEDEL                                                     
         BZ    DISR92D                                                          
         NI    NBRKCNTL-NBRKEY+IOKEY,X'FF'-X'80'                                
         GOTO1 AIO,DIRWRT                                                       
*                                                                               
DISR92D  GOTO1 AIO,FILGETU2+IORDEL                                              
         BE    DISR92F                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
DISR92F  L     R0,AIOAREA2                                                      
         L     RE,AIOAREA3                                                      
         LA    R1,L'IOAREA1                                                     
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         GOTO1 AIO,FILPUT2                 YES - PUT THE RECORD                 
         BE    DISRX                                                            
         DC    H'0'                                                             
*                                                                               
DISR95   GOTO1 AIO,FILADD3                                                      
*                                                                               
DISRX    NI    SVFLAG,255-SVFIRST                                               
         B     EXIT                                                             
         SPACE 2                                                                
DRCLC    CLC   8(0,R4),SPACES      EXECUTED INSTRUCTIONS                        
DROC     OC    8(0,R4),8(R4)                                                    
DRXC     XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
***********************************************************************         
* GET MIN AND MAX VALUES FOR TOLERANCE COMPARISONS                    *         
***********************************************************************         
         SPACE 1                                                                
MINMAX   NTR1  ,                                                                
         LA    R4,SVMINMAX         SET MIN AND MAX DEMO VALUES                  
         LA    R8,SVBKS            DETERMINED BY THE PCT TOLERANCE              
         LA    R9,SVDEMVAL                                                      
         LA    R0,4                                                             
*                                                                               
MINMAX2  XC    0(8,R4),0(R4)                                                    
         OC    0(2,R8),0(R8)                                                    
         BZ    MINMAX4                                                          
         L     RF,0(R9)                                                         
         LH    R1,SVPCT                                                         
         SLDL  RE,1                                                             
         MR    RE,R1                                                            
         D     RE,=F'100'          TO TWO DECIMAL PLACES                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         L     R1,0(R9)                                                         
         MH    R1,=H'10'                                                        
         LR    RE,R1                                                            
         SR    R1,RF                                                            
         ST    R1,0(R4)                                                         
         AR    RE,RF                                                            
         ST    RE,4(R4)                                                         
*                                                                               
MINMAX4  LA    R4,8(R4)                                                         
         LA    R8,2(R8)                                                         
         LA    R9,4(R9)                                                         
         BCT   R0,MINMAX2                                                       
*                                                                               
         XC    SVPMINMX(8),SVPMINMX    SET PROJECTED TOLERANCE VALUES           
         L     RF,SVPROJ                                                        
         SLDL  RE,1                                                             
         LH    R1,SVPCT                                                         
         MR    RE,R1                                                            
         D     RE,=F'100'          TO TWO DECIMAL PLACES                        
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         L     R1,SVPROJ                                                        
         SLL   R1,1                                                             
         SRL   R1,1                                                             
         MH    R1,=H'10'                                                        
         LR    RE,R1                                                            
         SR    R1,RF                                                            
         ST    R1,SVPMINMX                                                      
         AR    RE,RF                                                            
         ST    RE,SVPMINMX+4                                                    
*                                                                               
MINMAXX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RATING SERVICE                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSRC   LR    R0,RE                                                            
         MVC   APBYTE,SVRTGSVC                                                  
         CLI   APBYTE,0                                                         
         BNE   *+10                                                             
         MVC   APBYTE,CLTSRC                                                    
         LA    R1,LEDSRCH                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R1,ROTSRCH                                                       
         OI    6(R1),FVOXMT                                                     
         MVC   8(3,R1),=C'NSI'                                                  
         CLI   APBYTE,C'N'                                                      
         BE    *+10                                                             
         MVC   8(3,R1),=C'ARB'                                                  
         CLI   CUDMED,C'C'                                                      
         BNE   DISSRCX                                                          
         MVC   8(3,R1),=C'CSI'                                                  
         CLI   APBYTE,C'N'                                                      
         BE    DISSRCX                                                          
         MVC   8(3,R1),=C'BBM'                                                  
*                                                                               
DISSRCX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE BOOKS                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISBKS   NTR1                                                                   
         LA    R8,LEDBK1H                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R8,ROTBK1H                                                       
         LA    R0,4                                                             
         LA    R4,SVBKS                                                         
         LA    R9,C'1'                                                          
         MVI   APWORK+2,1                                                       
*                                                                               
DISB2    XC    8(L'LEDBK1,R8),8(R8)                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DISB4                                                            
         MVC   APWORK(2),0(R4)                                                  
         NI    APWORK+1,X'FF'-X'80'  TAKE OFF OLYMPIC DATA BIT                  
         GOTO1 VDATCON,APPARM,(3,APWORK),(6,10(R8))                             
*                                                                               
         TM    1(R4),X'80'         OLYMPIC DATA?                                
         BZ    DISB4                                                            
         MVC   8(6,R8),10(R8)      YES, LEFT-ALIGN THE DATE                     
         MVI   14(R8),C'O'                                                      
         MVI   15(R8),C' '                                                      
*                                                                               
DISB4    OI    6(R8),FVOXMT                                                     
         LA    R4,2(R4)                                                         
         ZIC   RF,0(R8)                                                         
         AR    R8,RF                                                            
         LA    R9,1(R9)                                                         
         BCT   R0,DISB2                                                         
*                                                                               
DISBX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE PERCENT FIELD FOR ROTATIONAL ANALYSIS                   *         
* OUTPUT : LCHG = LPCT IF THE PERCENTAGE CHANGES                     *          
*          CC EQ OK                                                   *         
*             NE ERROR                                                *         
***********************************************************************         
         SPACE 1                                                                
PCT      NTR1  ,                                                                
         GOTO1 AFVAL,ROTPCTH       VALIDATE PERCENT FIELD                       
         BH    PCTX                                                             
         BE    PCT2                                                             
         MVC   ROTPCT(3),=C'5.0'   MISSING - SET TO 5%                          
         OI    ROTPCTH+6,FVOXMT                                                 
         CLC   SVPCT,=H'50'        TEST CHANGE                                  
         BE    PCTX                                                             
         MVC   SVPCT,=H'50'        YES                                          
         OI    LCHG,LPCT                                                        
         B     PCTX                                                             
*                                                                               
PCT2     XC    APPARM+4(4),APPARM+4   PRESENT - VALIDATE IT                     
         MVC   APPARM+7(1),FVILEN                                               
         GOTO1 VCASHVAL,APPARM,(1,FVIFLD)                                       
         CLI   0(R1),X'FF'                                                      
         BE    PCT9                                                             
         L     RE,4(R1)                                                         
         LTR   RE,RE                                                            
         BNP   PCT9                                                             
         CH    RE,=H'1000'                                                      
         BNL   PCT9                                                             
         CLC   SVPCT,6(R1)                                                      
         BE    PCTX                                                             
         MVC   SVPCT,6(R1)                                                      
         OI    LCHG,LPCT                                                        
         B     PCTX                                                             
*                                                                               
PCT9     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
PCTX     CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT THE DEMO FIELD                                              *         
* OUTPUT : LCHG = LDEMO IF THE DEMO CHANGES                           *         
*          CC EQ OK                                                   *         
*             NE ERROR                                                *         
***********************************************************************         
         SPACE 1                                                                
DEMO     NTR1  ,                                                                
         LA    R4,LEDDEMH                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R4,ROTDEMH                                                       
         LR    R1,R4                                                            
         GOTO1 AFVAL               VALIDATE THE DEMO FIELD                      
         BH    DEMOX                                                            
         BE    DEMO2                                                            
         MVC   8(L'LEDDEM,R4),LDNAME    MISSING-USE PRIMARY DEMO                
         OI    6(R4),FVOXMT                                                     
         CLC   SVDEM,LTGTDEM       TEST ALREADY PRIMARY DEMO                    
         BE    DEMOX                                                            
         MVC   SVDEM,LTGTDEM       NO                                           
         OI    LCHG,LDEMO                                                       
         B     DEMOX                                                            
*                                                                               
DEMO2    LA    R0,ESTUSRNM                                                      
         GOTO1 VDEMOVAL,APPARM,(1,(R4)),(1,APDUB),(C'S',DBLOCK),(R0)            
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     DEMOX                                                            
         CLC   SVDEM(3),APDUB      TEST DEMO HAS CHANGED                        
         BE    DEMOX                                                            
         MVC   SVDEM(3),APDUB      YES                                          
         MVI   SVDEM+3,X'FF'                                                    
         OI    LCHG,LDEMO                                                       
         B     DEMOX                                                            
*                                                                               
DEMOX    CLC   FVMSGNO,=AL2(FVFOK)                                              
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
         GOTO1 AFVAL,LEDCSTH                                                    
         BH    COSTX2                                                           
         BL    COSTX1                                                           
         ZIC   RF,FVILEN                                                        
         ST    RF,APPARM+4                                                      
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   APPARM,X'FF'                                                     
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
         CLI   LEDUPGH+5,0         TEST UPGRADE ENTERED                         
         BE    UPGRX1                                                           
         MVI   APFLAG,X'F8'                                                     
         GOTO1 AVALUPG,LEDUPGH     VALIDATE UPGRADE FIELD                       
         BNE   UPGRX2              ERROR                                        
         MVI   FVINDX,0                                                         
         ICM   R4,15,LAUPGEL       TEST FOR UPGRADE ELEM                        
         BZ    UPGR2                                                            
         USING NBRUPEL,R4                                                       
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
         MVC   NBRUPINP(L'LEDUPG),LEDUPG                                        
         MVC   NBRSUPUT,APWORK+16                                               
         MVC   NBRSUSHR,APWORK+17                                               
         MVC   SVUPFILE,NBRUPFIL   SAVE UPGRADE VALUES                          
         MVC   SVUPGRD,NBRUPEXP                                                 
         MVC   SVUPFRBK,NBRUPOBK                                                
         XC    SVUPFBL,SVUPFBL                                                  
         CLI   NBRUPLEN,51                                                      
         BL    *+10                                                             
         MVC   SVUPFBL,NBRUPBKL                                                 
         MVC   SVUPINP,NBRUPINP                                                 
         MVC   SVUPPUT,NBRSUPUT                                                 
         MVC   SVUPSHR,NBRSUSHR                                                 
         OI    LCHG,LUPG                                                        
         OC    LAUPGEL,LAUPGEL                                                  
         BNZ   UPGR6                                                            
         L     R1,AIOAREA3                                                      
         GOTO1 AADDELS             ADD NEW UPGRADE ELEM                         
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
         CLC   NBRODODY,APWORK+11              YES - TEST FOR CHANGES           
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
         L     R1,AIOAREA3                                                      
         GOTO1 AADDELS             ADD NEW OVERRIDE ELEM                        
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
         EJECT                                                                  
***********************************************************************         
* INSPECT LEAD-IN/LEAD-OUT TIMES FIELDS                               *         
* INPUT  : R1=A(TIMES FIELD HEADER)                                   *         
* OUTPUT : BTIMES SET TO TIMES                                        *         
*          CC EQ  - OK                                                *         
*             LO  - BAD ERROR                                         *         
*             HI  - DAYS NOT FOUND                                    *         
***********************************************************************         
         SPACE 1                                                                
LEADTM   NTR1                                                                   
         GOTO1 AFVAL               TEST ANYTHING ENTERED                        
         BH    LEADTMX2                                                         
         BL    LEADTMX1                                                         
         GOTO1 AVALTIM             YES-VALIDATE                                 
         B     LEADTMX2                                                         
*                                                                               
LEADTMX1 CR    RA,RB                                                            
         B     EXIT                                                             
*                                                                               
LEADTMX2 CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET DEFAULT LEAD-IN AND LEAD-OUT TIMES                              *         
* INPUT  : SVTIMES=TIMES                                              *         
* OUTPUT : SVDFLITM=LEAD-IN TIMES                                     *         
*          SVDFLOTM=LEAD-OUT TIMES                                    *         
***********************************************************************         
         SPACE 1                                                                
GETLTIM  LR    R0,RE                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,SVTIMES        START TIME                                   
         D     RE,=F'100'                                                       
         LR    R1,RF               R1=RF=HOURS                                  
         LR    R8,RE               R8=RE=MINUTES                                
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'           ROUND MINUTES DOWN TO QTR HOUR               
         SR    R8,RE                                                            
         LR    RF,R1                                                            
         MH    RF,=H'100'          HOURS X 100                                  
         AR    RF,R8               + MINUTES                                    
         STCM  RF,3,SVDFLITM+2     SAVE LEAD-IN END TIME                        
         S     R8,=F'15'           SUBTRACT 15 MIN                              
         BNM   GETLTIM2                                                         
         LA    R8,60(R8)           BACK TO HOUR BEFORE                          
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNM   GETLTIM2                                                         
         LA    R1,24(R1)                                                        
*                                                                               
GETLTIM2 MH    R1,=H'100'          HOURS X 100                                  
         AR    R1,R8               + MINUTES                                    
         STCM  R1,3,SVDFLITM       SAVE LEAD-IN START TIME                      
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,SVTIMES+2      END TIME                                     
         BNZ   *+8                                                              
         ICM   RF,3,SVTIMES        ZERO-FORCE END TO START                      
         D     RE,=F'100'                                                       
         LR    R1,RF               R1=RF=HOURS                                  
         LR    R8,RE               R8=RE=MINUTES                                
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'           ROUND MINUTES UP TO QTR HOUR                 
         LNR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RE,15(RE)                                                        
         AR    R8,RE                                                            
         CH    R8,=H'60'                                                        
         BL    *+10                                                             
         SR    R8,R8                                                            
         LA    R1,1(R1)                                                         
         LR    RF,R1                                                            
         MH    RF,=H'100'          HOURS X 100                                  
         AR    RF,R8               + MINUTES                                    
         STCM  RF,3,SVDFLOTM       SAVE LEAD-OUT START TIME                     
         AH    R8,=H'15'           ADD 15 MIN                                   
         CH    R8,=H'60'                                                        
         BL    GETLTIM4                                                         
         SR    R8,R8               NEXT HOUR                                    
         LA    R1,1(R1)                                                         
         CH    R1,=H'24'                                                        
         BNH   GETLTIM4                                                         
         SH    R1,=H'24'                                                        
*                                                                               
GETLTIM4 MH    R1,=H'100'          HOURS X 100                                  
         AR    R1,R8               + MINUTES                                    
         STCM  R1,3,SVDFLOTM+2     SAVE LEAD-OUT END TIME                       
*                                                                               
GETLTIMX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET DAYS/TIMES LIST FOR DEMO LOOKUPS                                *         
***********************************************************************         
         SPACE 1                                                                
GETDTLST LR    R0,RE                                                            
         XC    SVDTLST,SVDTLST                                                  
         LA    R8,SVDTLST                                                       
         MVC   0(1,R8),SVDAYS      1ST IS BUY DAYS/TIMES                        
         MVC   1(4,R8),SVTIMES                                                  
*                                                                               
         MVC   5(1,R8),SVLIDAYS    2ND IS LEAD-IN TIMES                         
         MVC   6(2,R8),SVLITIME                                                 
         MVC   8(2,R8),SVLITIME+2                                               
*                                                                               
         MVC   10(1,R8),SVLODAYS   3RD IS LEAD-OUT TIMES                        
         MVC   11(2,R8),SVLOTIME                                                
         MVC   13(2,R8),SVLOTIME+2                                              
*                                                                               
         MVC   15(1,R8),SVDAYS     4TH IS ROTATION + LEAD-IN/LEAD-OUT           
         MVC   16(2,R8),SVLITIME                                                
         CLC   SVLITIME(2),SVTIMES                                              
         BNH   *+10                                                             
         MVC   16(2,R8),SVTIMES                                                 
         MVC   18(2,R8),SVLOTIME+2                                              
         CLC   SVLOTIME+2(2),SVTIMES+2                                          
         BNL   *+10                                                             
         MVC   18(2,R8),SVTIMES+2                                               
*                                                                               
         MVC   20(1,R8),SVDAYS     5TH IS IN-BREAK AVERAGE                      
         SR    RF,RF                                                            
         ICM   RF,3,SVDFLITM+2     I MIN BEFORE END OF DEFAULT LEAD-IN          
         BAS   R9,MINBEFOR                                                      
         STCM  RF,3,21(R8)                                                      
         XC    23(2,R8),23(R8)                                                  
*                                                                               
         MVC   25(1,R8),SVLODAYS   6TH IS OUT-BREAK AVERAGE                     
         SR    RF,RF                                                            
         ICM   RF,3,SVDFLOTM       1 MIN AFTER START OF DEFAULT                 
         LA    RF,1(RF)            LEAD-OUT                                     
         STCM  RF,3,26(R8)                                                      
         XC    28(2,R8),28(R8)                                                  
*                                                                               
DTLSTX   LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
MINBEFOR SR    RE,RE               GET MINUTE BEFORE QTR HOUR END               
         D     RE,=F'100'          TIME IS IN RF                                
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BNM   MINBEFO2                                                         
         LA    RE,60(RE)                                                        
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   MINBEFO2                                                         
         LA    RF,24(RF)                                                        
MINBEFO2 MH    RF,=H'100'                                                       
         AR    RF,RE               TIME RETURNED IN RF                          
         BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY HALF HOURS                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISHH    NTR1                                                                   
         ZIC   R8,SVHH                                                          
         LA    R9,4                DISPLAY 4 HALF HOURS                         
         LH    R3,SVNUMHH          UNLESS THERE ARE LESS THAN 4 LEFT            
         SR    R3,R8                                                            
         LA    R3,1(R3)                                                         
         CR    R3,R9                                                            
         BNH   *+6                                                              
         LR    R3,R9               R3=N'HALF HOURS ON THIS SCREEN               
         MH    R8,=H'5'                                                         
         LA    R8,SVDTLST(R8)      POINT TO CURRENT HALF HOUR                   
         LA    R4,ROTHH1H                                                       
*                                                                               
DISHH2   XC    8(L'ROTHH1,R4),8(R4)                                             
         LTR   R3,R3                                                            
         BNP   DISHH4                                                           
         GOTO1 VUNTIME,APPARM,1(R8),8(R4) DISPLAY THE HALF HOUR                 
*                                                                               
DISHH4   OI    6(R4),FVOXMT                                                     
         LA    R4,ROTHH2-ROTHH1(R4)                                             
         LA    R8,5(R8)                                                         
         BCTR  R3,0                                                             
         BCT   R9,DISHH2                                                        
*                                                                               
DISHHX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET HALF HOURS FOR ROTATIONAL ANALYSIS                              *         
***********************************************************************         
GETHH    NTR1                                                                   
         XC    SVDTLST,SVDTLST                                                  
         LA    R4,12                                                            
         LA    R8,SVDTLST                                                       
         MVC   0(1,R8),SVDAYS                                                   
         MVC   1(4,R8),SVTIMES     FIRST IS WHOLE ROTATION                      
         LA    R8,5(R8)                                                         
         MVC   0(1,R8),SVDAYS                                                   
         MVC   1(2,R8),SVTIMES                                                  
         XR    R3,R3                                                            
         OC    SVTIMES+2(2),SVTIMES+2                                           
         BZ    GETHH3                                                           
         ICM   R3,3,SVTIMES                                                     
*                                                                               
GETHH1   MVC   0(1,R8),SVDAYS      2ND IS 1ST HALF HOUR, ETC.                   
         STCM  R3,3,1(R8)                                                       
         XR    R2,R2                                                            
         D     R2,=F'100'          R2=MINUTES, R3=HOURS                         
*                                                                               
         LA    R1,30                                                            
         CR    R2,R1               TIME IN 1ST HALF/2ND HALF OF HOUR?           
         BL    GETHH2                                                           
         SR    R1,R1               2ND HALF OF THE HOUR                         
         LA    R3,1(R3)            SO BUMP THE HOUR                             
*                                                                               
         CHI   R3,24               WENT FROM PM TO PAST 1259AM?                 
         BNH   GETHH2                                                           
         AHI   R3,-24              YES                                          
*                                                                               
GETHH2   MHI   R3,100              FIGURE OUT ROUNDED END TIME                  
         AR    R3,R1                                                            
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,SVTIMES+2                                                   
         CLC   SVTIMES(2),SVTIMES+2  HIGH-TEST TIMES CROSSES MIDNIGHT           
         BNH   GETHH2A             NO-FORCE TO TIMES END                        
         CHI   R3,2400                                                          
         BL    GETHH2A                                                          
         AHI   R0,2400             YES, ADJUST                                  
*                                                                               
GETHH2A  CR    R3,R0               COMPARE HALF-HOUR END TO TIMES END           
         BNH   GETHH3                                                           
         CLC   SVTIMES(2),SVTIMES+2  HIGH-TEST TIMES CROSSES MIDNIGHT           
         BNH   GETHH2B             NO-FORCE TO TIMES END                        
         CHI   R3,600                                                           
         BL    GETHH2B                                                          
         CHI   R3,2400             YES-FORCE TO TIMES END ONLY IF               
         BL    GETHH3                  HALF HOUR END IS EARLY AM                
GETHH2B  LR    R3,R0                                                            
*                                                                               
GETHH3   STCM  R3,3,3(R8)                                                       
         LTR   R3,R3                                                            
         BZ    GETHH4                                                           
         CLM   R0,3,3(R8)          TEST REACHED END TIME                        
         BE    GETHH4                                                           
         LA    R8,5(R8)            NO-GO TO NEXT HALF HOUR                      
         BCT   R4,GETHH1                                                        
         LA    R4,1                                                             
*                                                                               
GETHH4   AHI   R4,-13                                                           
         LPR   R4,R4                                                            
         STH   R4,SVNUMHH          SAVE N'HALF-HOURS                            
*                                                                               
GETHHX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSPECT BOOK FIELDS                                                 *         
* OUTPUT : LCHG = LBK IF BOOKS WERE CHANGED                           *         
*          CC EQ  - OK                                                *         
*             NE  - ERROR                                             *         
***********************************************************************         
         SPACE 1                                                                
BOOKS    NTR1                                                                   
         LA    R4,LEDBK1H                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R4,ROTBK1H                                                       
         LA    R8,SVBKS                                                         
         LA    R0,4                                                             
*                                                                               
BOOK2    LR    R1,R4               VALIDATE BOOK FIELD                          
         GOTO1 AFVAL                                                            
         BH    BOOKX                                                            
         BE    BOOK4                                                            
         OC    0(2,R8),0(R8)                                                    
         BZ    BOOK6                                                            
         XC    0(2,R8),0(R8)                                                    
         OI    LCHG,LBK                                                         
         B     BOOK6                                                            
*                                                                               
BOOK4    MVI   APWORK+16,C' '                                                   
         LA    R1,FVIFLD+5         TEST FOR OLYMPIC                             
         CLI   0(R1),C'O'                                                       
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         CLI   0(R1),C'O'                                                       
         BNE   BOOK5                                                            
         MVI   APWORK+16,C'O'                                                   
         MVI   0(R1),C' '                                                       
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
         CLI   APWORK+16,C'O'                                                   
         BNE   *+8                                                              
         OI    APWORK+6+1,X'80'    OLYMPIC OVERRIDE                             
*                                                                               
         CLC   0(2,R8),APWORK+6    COMPARE TO OLD VALUE                         
         BE    BOOK6                                                            
         MVC   0(2,R8),APWORK+6    SAVE BOOK YR/MN                              
         OI    LCHG,LBK                                                         
*                                                                               
BOOK6    SR    RF,RF               NEXT BOOK                                    
         ICM   RF,1,0(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,RF                                                            
         LA    R8,2(R8)                                                         
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
ENOC     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
ECMSEQ   MVC   FVMSGNO,=AL2(FVCMSEQ)                                            
         LA    R1,LEDNUMH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
ESUB     MVC   FVMSGNO,=AL2(FVISDPT)                                            
         B     EXIT                                                             
*                                                                               
ENOSLN   MVC   FVMSGNO,=AL2(FVNOSLN)                                            
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
AVALRTG  DS    A                                                                
ADEMUP   DS    A                                                                
ASDEMUP  DS    A                                                                
AGETDEMS DS    A                                                                
AGETBKS  DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
CMT1     DC    CL8'PF2=Tran'                                                    
CMT2     DC    CL36'PF4=Frst PF5=Curr PF6=Next PF12=Quit'                       
SPACES   DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
DISKEYF  NTR1  BASE=*,LABEL=*                                                   
         MVC   LSVDPTLN,BDPT       SAVE DAYPART/LENGTH                          
         MVC   IOKEY,APRECKEY      GET THE RECORD                               
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
         MVC   LEDMED,QMED         MEDIA                                        
*                                                                               
         LA    R1,BBYR             BUYER                                        
         ICM   R1,8,=X'B'                                                       
         GOTO1 AGETBYR                                                          
         MVC   LEDBYR,QBYR                                                      
*                                                                               
         GOTO1 AGETCM,BCMSEQ       CAMPAIGN                                     
         MVC   LEDNUM,QCAM                                                      
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
         MVC   LEDSTA(5),APWORK+9  STATION                                      
         OI    LEDSTAH+6,X'80'                                                  
         MVI   LEDSTAH+5,5                                                      
         CLI   LEDSTA+4,C' '                                                    
         BE    *+16                                                             
         CLI   LEDSTA+4,C'T'                                                    
         BNE   DISK10                                                           
         MVI   LEDSTA+4,C' '                                                    
         MVI   LEDSTAH+5,4                                                      
*                                                                               
DISK10   CLI   LEDSTA,C'0'                                                      
         BL    *+14                                                             
         MVI   LEDSTA+4,C'/'                                                    
         MVC   LEDSTA+5(3),APWORK+9+5                                           
*                                                                               
         CLI   0(R3),NBRKTYPQ                                                   
         BH    DISK20                                                           
         USING NBRKEY,R3                                                        
         ZIC   R1,NBRKNBSQ         SEQ NUMBER FOR MANUAL BUY REVISION           
         LA    R2,LEDSEQH                                                       
         CLI   NBRKNBSQ,0                                                       
         BNE   DISK25                                                           
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         IC    R1,NBRSBYLN                                                      
         LA    R2,LEDLINH                                                       
         DROP  RE                                                               
         B     DISK25                                                           
         USING BUYKEY,R3                                                        
DISK20   ZIC   R1,BUYKBUY          BUYLINE NUMBER FOR AGENCY BUY RECORD         
         LA    R2,LEDLINH                                                       
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
         MVC   LEDDAY,QDAYS                                                     
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
         MVC   LEDTIM,QTIMES                                                    
*********                                                                       
* DISPLAY THE DAYPART AND SPOT LENGTH                                           
*********                                                                       
         CLI   0(R3),NBRKTYPQ      BUY REVISION RECORD?                         
         BH    DISK50                                                           
         USING NBRKEY,R3                                                        
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         MVC   LEDDLN(1),NBRSDYPT                                               
         ZIC   RF,NBRSSLN                                                       
         B     DISK55                                                           
         DROP  RE                                                               
         USING BUYKEY,R3                                                        
DISK50   MVC   LEDDLN(1),BDDAYPT                                                
         ZIC   RF,BDSEC                                                         
         DROP  R3                                                               
DISK55   CVD   RF,APDUB            DISPLAY SPOT LENGTH                          
         OI    APDUB+7,X'0F'                                                    
         LA    R1,LEDDLN+1                                                      
         CLI   LEDDLN,C'1'                                                      
         BL    *+12                                                             
         MVI   0(R1),C'/'                                                       
         LA    R1,1(R1)                                                         
         UNPK  0(3,R1),APDUB                                                    
         CLI   0(R1),C'0'                                                       
         BNE   *+14                                                             
         MVC   0(2,R1),1(R1)                                                    
         MVI   2(R1),C' '                                                       
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DISKX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**B28X**,RA                                                    
         L     RC,APALOCAL                                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALRTG                                                           
         B     DEMUP                                                            
         B     SDEMUP2                                                          
         B     GETDEMS                                                          
         B     GETBKS                                                           
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATING FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALRTG   L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         LA    R1,LEDRTGH                                                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+8                                                              
         LA    R1,ROTRPJH                                                       
         TM    FVIIND-FVIHDR(R1),FVIVAL   TEST RATING FIELD CHANGED             
         BO    VALRTGX                                                          
         OI    FVIIND-FVIHDR(R1),FVIVAL   YES-SET PREVIOUSLY VALIDATED          
         GOTO1 AFVAL               VALIDATE RATING FIELD                        
         BNE   VALRTGX                                                          
*                                                                               
         ICM   R1,15,LADEMEL       LOCATE DEMO IN BWS DETAIL RECORD             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING NBRDMEL,R1                                                       
         ZIC   RF,NBRDMLEN                                                      
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    RE,L'NBRDMDMO                                                    
         LA    R4,NBRDMDMO                                                      
         CLC   1(2,R4),SVDEM+1                                                  
         BE    VALRTG2             R4 = A(DEMO ENTRY IN DEMO ELEMENT)           
         BXLE  R4,RE,*-10                                                       
*                                                                               
         XC    APELEM,APELEM       NOT FOUND - ADD DEMO TO ELEMENT              
         ZIC   RF,NBRDMLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)     SAVE THE ELEMENT                             
         DROP  R1                                                               
*                                                                               
         L     R1,AIOAREA3                                                      
         GOTO1 ADELELS             DELETE IT FROM RECORD                        
         ZIC   RF,APELEM+1                                                      
         LA    RF,L'NBRDMDMO(RF)   LENGTHEN ELEMENT BY ONE DEMO                 
         STC   RF,APELEM+1                                                      
         GOTO1 AADDELS             ADD BACK THE ELEMENT                         
         MVC   0(3,R4),SVDEM       MOVE IN THE DEMO CODE                        
*                                                                               
VALRTG2  XC    APDUB,APDUB                                                      
         XC    LDEMOLD,LDEMOLD     SAVE CURRENT DEMO VALUE                      
         MVC   LDEMOLD+1(3),5(R4)                                               
         ZIC   R8,FVILEN                                                        
         LR    RF,R8                                                            
         LA    R1,FVIFLD           REMOVE THE * IF ANY                          
*                                                                               
VALRTG4  CLI   0(R1),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   0(R1),C'X'          TEST FOR X                                   
         BE    *+16                YES-REMOVE THE OVERRIDE                      
         LA    R1,1(R1)                                                         
         BCT   RF,VALRTG4                                                       
         B     VALRTG6                                                          
         MVC   LDEMLST(3),0(R4)    SINGLE UPGRADE FOR RATING                    
         MVI   LDEMLST+3,X'FF'                                                  
         BAS   RE,SDEMUP                                                        
         MVC   4(4,R4),LDEMVAL                                                  
         MVC   SVPROJ(4),LDEMVAL                                                
         OI    LCHG,LRTG                                                        
         B     VALRTG8                                                          
*                                                                               
VALRTG6  GOTO1 VCASHVAL,APPARM,(1,FVIFLD),(R8)                                  
         CLI   APPARM,X'FF'                                                     
         BE    VALRTG99                                                         
         CLC   5(3,R4),APPARM+5    TEST FOR RATING CHANGE                       
         BNE   *+12                                                             
         TM    LFLAG2,LTIMOVR+LDAYOVR  OR OVERRIDDEN DAYS/TIMES                 
         BZ    VALRTGX                                                          
         MVC   4(4,R4),APPARM+4    YES - MOVE IN NEW RATING                     
         OI    4(R4),NBRDMOOV            OVERRIDE INDICATOR                     
         OI    LCHG,LRTG                                                        
         MVC   SVPROJ(4),APPARM+4                                               
         OI    SVPROJ,X'80'              INDICATE OVERRIDE                      
*                                                                               
VALRTG8  TM    INFIND,INFINOAD    TEST OPTION TO NOT AUTO ADJUST DEMOS          
         BO    VALRTGX                                                          
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT   NO-TEST AUTO DEMO           
         BZ    VALRTGX                                 ADJUSTMENTS              
         OC    LDEMOLD,LDEMOLD     YES-TEST OLD DEMO VALUE = 0                  
         BZ    VALRTG98            YES-ERROR                                    
         BAS   RE,DEMADJ           NO-DO THE ADJUSTMENTS                        
         B     VALRTGX                                                          
*                                                                               
VALRTG98 MVC   FVMSGNO,=AL2(FVIDADJ)                                            
         B     VALRTGX                                                          
*                                                                               
VALRTG99 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALRTGX  CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AUTO DEMO ADJUSTMENT ROUTINE                                        *         
* INPUT  : R4 = A(NEW DEMO ENTRY IN DEMO ELEMENT)                     *         
*          LADEMEL = A(DEMO ELEMENT)                                  *         
*          LDEMOLD = OLD RATING                                       *         
***********************************************************************         
         SPACE 1                                                                
DEMADJ   NTR1                                                                   
         NI    LFLAG,255-LADJALL                                                
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
         BNE   DEMAX               NO - NO ADJUSTMENTS                          
*                                                                               
DEMA4    L     R1,LDEMOLD          CALCULATE PCT ADJUSTMENT                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,7,5(R4)                                                       
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,LDEMADJ          LDEMADJ = PCT ADJUSTMENT                     
*                                                                               
         L     R5,LADEMEL          SCAN ALL DEMOS IN DEMO ELEMENT               
         ZIC   R3,1(R5)                                                         
         AR    R3,R5                                                            
         BCTR  R3,0                                                             
         LA    R5,NBRDMDMO-NBRDMEL(R5)                                          
         LA    R2,L'NBRDMDMO                                                    
*                                                                               
DEMA6    CLC   1(2,R5),1(R4)       TEST OUR DEMO                                
         BE    DEMA12              YES - NO ADJ                                 
         TM    4(R5),NBRDMOOV      TEST ALREADY MANUALLY OVERRIDDEN             
         BO    DEMA12              YES - NO ADJ                                 
         TM    LFLAG,LADJIMP       TEST TARGET IMPRESSION ADJUST                
         BZ    DEMA8                                                            
         CLI   1(R5),C'I'          YES - TEST ITS THE IMP WE WANT               
         BNE   DEMA8                                                            
         CLC   2(1,R5),2(R4)                                                    
         BE    DEMA10                    YES - ADJUST                           
*                                                                               
DEMA8    TM    LFLAG,LADJALL       TEST ADJUST ALL                              
         BZ    DEMA12              NO                                           
         CLI   1(R5),C'I'          YES - TEST ITS AN IMPRESSION                 
         BNE   DEMA10                    NO - GO AND ADJUST                     
         L     R8,LADEMEL                YES - CHECK THAT ITS RATING            
         LA    R8,NBRDMDMO-NBRDMEL(R8)         HAS NOT BEEN MANUALLY            
         CLI   1(R8),C'R'                      OVERRIDDEN                       
         BNE   *+14                                                             
         CLC   2(1,R8),2(R5)                                                    
         BE    *+12                                                             
         BXLE  R8,R2,*-18                                                       
         B     DEMA10                                                           
         TM    4(R8),NBRDMOOV                                                   
         BO    DEMA12                                                           
*                                                                               
DEMA10   SR    R8,R8               DO THE ADJUSTMENT                            
         ICM   R8,7,5(R5)                                                       
         SR    R9,R9                                                            
         SRDA  R8,31                                                            
         M     R8,LDEMADJ                                                       
         D     R8,=F'1000'                                                      
         AH    R9,=H'1'                                                         
         SRA   R9,1                                                             
         ST    R9,4(R5)            STORE ADJUSTED DEMO IN ELEMENT               
*                                                                               
DEMA12   BXLE  R5,R2,DEMA6         NEXT DEMO                                    
*                                                                               
DEMAX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SINGLE DEMO UPGRADE ROUTINE                                         *         
* INPUT  : LDEMLST=SINGLE DEMO                                        *         
* OUTPUT : LDEMVAL=DEMO VALUE                                         *         
*          SVRTGSVC=ACTUAL RATING SERVICE                             *         
***********************************************************************         
         SPACE 1                                                                
SDEMUP   NTR1                                                                   
*                                                                               
SDEMUP2  L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         XC    LDEMVAL,LDEMVAL                                                  
         OC    SVUPGRD,SVUPGRD                                                  
         BZ    SDEMUPX                                                          
         LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         LA    RE,LIUN                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   SPUPSTA,QSTA                                                     
         MVC   SPUPDAY,SVDAYS                                                   
         MVC   SPUPTIM,SVTIMES                                                  
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
SDEMUP4  CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
*                                                                               
         TM    SPUPFBK+1,X'80'     OLYMPIC BOOK?                                
         BZ    *+12                                                             
         MVI   SPUPBTYP,C'O'       YES                                          
         NI    SPUPFBK+1,X'FF'-X'80'                                            
*                                                                               
         CLI   SVUPPUT,C'1'                                                     
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
         GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMLST,LDEMVAL                         
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
*                                                                               
SDEMUPX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DEMO UPGRADE ROUTINE                                                *         
***********************************************************************         
         SPACE 1                                                                
DEMUP    L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         XC    LDEMOVR,LDEMOVR                                                  
         OC    SVUPGRD,SVUPGRD     TEST UPGRADE EXISTS                          
         BZ    DEMUX                                                            
         NI    LFLAG,255-LDEMFRZ   BUILD DEMO OVERRIDE ELEMENT                  
         ICM   R8,15,LADEMEL                                                    
         BZ    DEMU14                                                           
         USING NBRDMEL,R8                                                       
         ZIC   RF,1(R8)                                                         
         AR    RF,R8                                                            
         BCTR  RF,0                                                             
         LA    RE,L'NBRDMDMO                                                    
         LA    R1,NBRDMDMO                                                      
         USING NBRDMDMO,R1                                                      
*                                                                               
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT   TEST AUTO DEMO              
         BZ    DEMU10                               ADJUST                      
         TM    CMPOPTS,CAMOAALL+CAMOATGT  TEST AUTOADJ=ALL/TGT                  
         BNZ   DEMU2                                                            
         CLI   SVDEM+1,C'I'        NO-TEST DEMO IS IMPRESSION                   
         BNE   DEMU10                 NO-DEMO IS NOT FROZEN                     
*                                                                               
DEMU2    TM    NBRDMDMO+4,NBRDMOOV TEST MANUAL OVERRIDE                         
         BZ    DEMU6                                                            
         TM    CMPOPTS,CAMOAALL+CAMOATGT  TEST AUTOADJ=ALL/TGT                  
         BZ    DEMU4                                                            
         MVC   APHALF,=X'D901'                                                  
         TM    CMPOPTS,CAMOAALL                                                 
         BO    *+10                                                             
         MVC   APHALF,ESTDEMS+1                                                 
         CLC   APHALF,NBRDMDMO+1   YES-TEST ADJUSTMENT DEMO                     
         BNE   DEMU4                                                            
         OI    LFLAG,LDEMFRZ       YES-FREEZE THE DEMO                          
         B     DEMU8                                                            
*                                                                               
DEMU4    CLI   SVDEM+1,C'I'        TEST DEMO IS IMPRESSION                      
         BNE   DEMU6                                                            
         CLI   NBRDMDMO+1,C'R'     YES - TEST THIS IS ITS RATING                
         BNE   DEMU6                                                            
         CLC   NBRDMDMO+2(1),SVDEM+2                                            
         BNE   DEMU6                                                            
         OI    LFLAG,LDEMFRZ       YES - FREEZE THE DEMO                        
         B     DEMU8                                                            
*                                                                               
DEMU6    BXLE  R1,RE,DEMU2                                                      
*                                                                               
DEMU8    L     R1,LADEMEL                                                       
         LA    R1,NBRDMDMO-NBRDMEL(R1)                                          
*                                                                               
DEMU10   CLC   NBRDMDMO+1(2),SVDEM+1  FIND DEMO IN DEMO ELEMENT                 
         BNE   DEMU12                                                           
         TM    LFLAG,LDEMFRZ          TEST FREEZE THE DEMO VALUE                
         BO    *+12                                                             
         TM    NBRDMDMO+4,NBRDMOOV    OR MANUAL OVERRIDE                        
         BZ    DEMU14                                                           
         MVI   LDEMOVR,OVERELEM       YES - BUILD OVERRIDE ELEM                 
         MVI   LDEMOVR+1,6                                                      
         MVC   LDEMOVR+2(2),NBRDMDMO+1                                          
         MVC   LDEMOVR+4(2),NBRDMDMO+6                                          
         TM    NBRDMDMO+4,NBRDMOOV    TEST THIS IS MANUAL OVERRIDE              
         BZ    DEMU14                                                           
         NI    LFLAG,255-LDEMFRZ      YES-NOT RESULT OF FREEZE                  
         OI    LFLAG,LRTGOVR          INDICATE MANUAL RATING OVERRIDE           
         B     DEMU14                                                           
*                                                                               
DEMU12   BXLE  R1,RE,DEMU10                                                     
         DROP  R1                                                               
*                                                                               
DEMU14   LA    R4,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUPL),SPDEMUPD                                      
         LA    RE,LIUN                                                          
         ST    RE,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   SPUPSTA,QSTA                                                     
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
         BNE   DEMU15                                                           
         CLI   CMPBKTYP,0                                                       
         BE    DEMU15                                                           
         MVC   SPUPBTYP,CMPBKTYP                                                
*                                                                               
DEMU15   CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
*                                                                               
         TM    SPUPFBK+1,X'80'     OLYMPIC BOOK?                                
         BZ    *+12                                                             
         MVI   SPUPBTYP,C'O'       YES                                          
         NI    SPUPFBK+1,X'FF'-X'80'                                            
*                                                                               
         CLI   SVUPPUT,C'1'                                                     
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
         LA    R2,6                R2 = NUMBER OF LOOKUPS                       
         CLI   ACSVACT,ACTROT                                                   
         BNE   *+16                                                             
         LH    R2,SVNUMHH                                                       
         LA    R2,1(R2)                                                         
         LA    R3,SVPRJPRG         R3 = A(PROGRAMS) - FOR ROTN ANAL             
         LA    R8,SVDTLST          R8 = A(DAYS/TIMES LIST)                      
         LA    R9,SVPROJ           R9 = A(DEMO VALUE AREA)                      
         XC    SVPROJ,SVPROJ       CLEAR DEMO VALUE AREA                        
         XC    SVLIPROG,SVLIPROG   CLEAR PROGRAM NAMES                          
         XC    SVLOPROG,SVLOPROG                                                
*                                                                               
DEMU16   MVC   SPUPDAY,0(R8)       SET DAYS/TIMES                               
         MVC   SPUPTIM,1(R8)                                                    
*                                                                               
         GOTO1 ASPDEMUP,APPARM,LDMUPBLK,SVDEM,(R9)                              
*                                                                               
         MVC   SVRTGSVC,SPUPACTS   SAVE ACTUAL RATING SERVICE                   
         XC    SPUPAOVR,SPUPAOVR   CLEAR DEMO OVERRIDE                          
         MVI   SVDEM,0             REMOVE OVERRIDE INDICATOR                    
         TM    LFLAG,LRTGOVR       TEST MANUAL OVERRIDE                         
         BZ    *+12                                                             
         OI    0(R9),X'80'         YES                                          
         NI    LFLAG,255-LRTGOVR                                                
         CLI   ACSVACT,ACTROT      SAVE PROGRAM FOR ROTATION ANALYSIS           
         BNE   DEMU17                                                           
         MVC   0(10,R3),SPUPPRG                                                 
         LA    R3,10(R3)                                                        
         B     DEMU18                                                           
*                                                                               
DEMU17   CH    R2,=H'5'            SAVE LEAD-IN AND LEAD-OUT                    
         BNE   *+10                PROGRAMMING                                  
         MVC   SVLIPROG,SPUPPRG                                                 
         CH    R2,=H'4'                                                         
         BNE   DEMU18                                                           
         MVC   SVLOPROG,SPUPPRG                                                 
*                                                                               
DEMU18   LA    R8,5(R8)            NEXT DAYS/TIMES                              
         LA    R9,4(R9)                                                         
         BCT   R2,DEMU16                                                        
*                                                                               
DEMUX    B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOOKUP THE DEMO VALUES FOR 4 BOOKS                       *         
***********************************************************************         
         SPACE 1                                                                
GETDEMS  XC    SPDEMLK,SPDEMLK                                                  
         LA    RE,LIUN                                                          
         ST    RE,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOM                                                    
         MVC   SPLKAGY,CUAALF                                                   
         MVC   SPLKMED,CUDMED                                                   
         MVC   SPLKCLI,QCLT                                                     
         MVC   SPLKSTA,QSTA                                                     
         MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKSRC,CLTSRC                                                   
*                                                                               
         CLI   SVRTGSVC,0          TEST SOURCE RETURNED FROM SPDEMUP            
         BE    *+14                                                             
         MVC   SPLKSRC,SVRTGSVC    YES-USE IT AND                               
         B     *+10                    SUPPRESS MARKET OVERRIDE                 
         MVC   SPLKUMK,MKTLKUP                                                  
         MVI   SPLKSVI,X'FF'                                                    
*                                                                               
         LA    R1,G1WPROF                                                       
         ST    R1,SPLKA1W                                                       
*        CLI   CUDMED,C'C'         TEST CANADA                                  
*        BNE   GETD1                                                            
*        TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
*        BZ    GETD1                                                            
*        XC    L1WPROF,L1WPROF     YES-PASS A(1W PROFILE)                       
*        LA    R1,L1WPROF                                                       
*        MVI   3(R1),C'Y'                                                       
*        ST    R1,SPLKA1W                                                       
*                                                                               
GETD1    LA    R1,SVDEM                                                         
         ST    R1,SPLKALST                                                      
         LA    R1,APFULL                                                        
         ST    R1,SPLKAVAL                                                      
         LA    R4,SVDTLST          R4 = A(DAYS/TIMES LIST)                      
         LA    R9,SVDEMVAL         R9 = A(DEMO VALUE AREA)                      
         XC    SVDEMVAL,SVDEMVAL   CLEAR DEMO VALUE AREA                        
*                                                                               
         CLI   ACSVACT,ACTROT      FOR ROTATION ANALYSIS,                       
         BNE   GETD2                                                            
         LH    R2,SVNUMHH          R2=N'DAYS/TIMES=N'HALFHOURS +1               
         LA    R2,1(R2)                                                         
         LA    R3,SVPROGS          R3=A(PROGRAM NAMES)                          
         B     GETD3                                                            
*                                                                               
GETD2    LA    R2,6                R2=N'DAYS/TIMES                              
         SR    R3,R3               NO PROGRAM NAMES YET                         
         XC    SVLIPRGS,SVLIPRGS                                                
         XC    SVLOPRGS,SVLOPRGS                                                
*                                                                               
GETD3    MVC   SPLKDAY,0(R4)       SET DAYS/TIMES                               
         MVC   SPLKTIM,1(R4)                                                    
         LA    R0,4                                                             
         LA    R8,SVBKS            R8 = A(BOOK LIST)                            
*                                                                               
GETD4    MVC   SPLKBTYP,STABKTYP                                                
         OC    0(2,R8),0(R8)                                                    
         BZ    GETD6                                                            
         MVC   SPLKDBK,0(R8)                                                    
         NI    SPLKOPT,X'FF'-SPLKOEXO                                           
         TM    1(R8),X'80'         OLYMPIC OVERRIDE?                            
         BZ    *+16                                                             
         OI    SPLKOPT,SPLKOEXO    YES                                          
         MVI   SPLKBTYP,C'O'                                                    
         NI    SPLKDBK+1,X'FF'-X'80'                                            
         XC    APFULL,APFULL                                                    
         GOTO1 VSPDEMLK,APPARM,(X'FF',SPDEMLK)                                  
         MVC   0(4,R9),APFULL      DEMO VALUE                                   
*                                                                               
GETD6    LTR   R3,R3               TEST NEED TO SAVE PROGRAM                    
         BZ    GETD7                                                            
         CLI   ACSVACT,ACTLED      YES                                          
         BNE   *+18                                                             
         MVC   0(16,R3),SPLKPRG                                                 
         LA    R3,16(R3)                                                        
         B     GETD7                                                            
         MVC   0(10,R3),SPLKPRG                                                 
         LA    R3,10(R3)                                                        
*                                                                               
GETD7    MVI   SPLKPRG,C' '                                                     
         MVC   SPLKPRG+1(L'SPLKPRG-1),SPLKPRG                                   
         LA    R8,2(R8)            NEXT BOOK                                    
         LA    R9,4(R9)                                                         
         BCT   R0,GETD4                                                         
*                                                                               
         LA    R4,5(R4)            NEXT DAYS/TIMES                              
         CLI   ACSVACT,ACTLED                                                   
         BNE   GETD8                                                            
         LA    R3,SVLIPRGS         SAVE LEAD-IN AND LEAD-OUT PROGRAMS           
         CH    R2,=H'6'                                                         
         BE    GETD8                                                            
         LA    R3,SVLOPRGS                                                      
         CH    R2,=H'5'                                                         
         BE    GETD8                                                            
         SR    R3,R3                                                            
*                                                                               
GETD8    BCT   R2,GETD3                                                         
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET THE BOOKS                                                       *         
***********************************************************************         
         SPACE 1                                                                
GETBKS   DS    0H                                                               
         OC    CMPBOOKS,CMPBOOKS   NONE - TEST FOR CAMPAIGN DEFINED BKS         
         BZ    *+14                                                             
         MVC   SVBKS,CMPBOOKS             YES - USE THOSE                       
         B     GETBX                                                            
         XC    DBLOCK,DBLOCK       NO CAMPAIGN DEFINED BOOKS -                  
         MVC   DBCOMFCS,ACOM       GET LATEST BOOK FROM DEMAND                  
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBAREC,AIOAREA3                                                  
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
         XC    SVBKS,SVBKS         CLEAR SAVED BOOKS FIELD                      
         LA    R4,SVBKS+6                                                       
         LA    R1,4                                                             
         MVC   0(2,R4),DBACTBK                                                  
         OC    SVUPFRBK,SVUPFRBK   TEST FOR OVERRIDE SHARE BOOK                 
         BZ    GETB12              NO - THEN INCLUDE LATEST BOOK                
*                                                                               
GETB2    MVC   0(2,R4),DBACTBK     DETERMINE ALL FOUR BOOKS                     
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
         ZIC   RF,DBACTBK                                                       
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
GETB12   MVC   DBACTBK,0(R4)                                                    
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         STC   RF,DBACTBK+1                                                     
         B     GETB14                                                           
         MVI   DBACTBK+1,12                                                     
         IC    RF,0(R4)                                                         
         BCTR  RF,0                                                             
         STC   RF,DBACTBK                                                       
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
OVERELEM EQU   X'DE'                                                            
         EJECT                                                                  
LOCALD   DSECT                                                                  
LASAVE   DS    A                                                                
LACOST   DS    A                                                                
LAUPGEL  DS    A                                                                
LAODTEL  DS    A                                                                
LADEMEL  DS    A                                                                
LDEMADJ  DS    F                                                                
LDEMOLD  DS    F                                                                
LDNAME   DS    CL7                                                              
LDMUPBLK DS    (SPDEMUPL)X                                                      
LTGTDEM  DS    XL4                                                              
LDEMOVR  DS    XL7                                                              
LSVDPTLN DS    XL2                                                              
LDEMLST  DS    XL4                                                              
LDEMVAL  DS    XL4                                                              
LDPT     DS    CL1                                                              
L1WPROF  DS    XL16                                                             
*                                                                               
LFLAG    DS    X                                                                
LADJIMP  EQU   X'80'                                                            
LADJALL  EQU   X'40'                                                            
LDEMFRZ  EQU   X'20'                                                            
LRTGOVR  EQU   X'10'                                                            
*                                                                               
LFLAG2   DS    X                                                                
LDAYOVR  EQU   X'80'                                                            
LNEWDAYS EQU   X'40'                                                            
LTIMOVR  EQU   X'20'                                                            
LNEWTIME EQU   X'10'                                                            
*                                                                               
LCHG     DS    X                                                                
LCOST    EQU   X'80'                                                            
LUPG     EQU   X'40'                                                            
LBK      EQU   X'20'                                                            
LPRG     EQU   X'10'                                                            
LRTG     EQU   X'08'                                                            
LLEAD    EQU   X'04'                                                            
LDEMO    EQU   X'02'                                                            
LPCT     EQU   X'01'                                                            
*                                                                               
BBUYLINE DS    XL1                                                              
BSEQNUM  DS    XL1                                                              
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
LILOL1D  DSECT                                                                  
         DS    XL2                                                              
LRTG1    DS    CL6                                                              
         DS    XL7                                                              
LRTG2    DS    CL6                                                              
         DS    XL7                                                              
LRTG3    DS    CL6                                                              
         DS    XL7                                                              
LRTG4    DS    CL6                                                              
         DS    XL2                                                              
LPROJECT DS    CL10                                                             
         DS    XL1                                                              
LCPP     DS    CL7                                                              
         SPACE 2                                                                
LILOL2D  DSECT                                                                  
LPRGBK1  DS    CL10                                                             
         DS    CL3                                                              
LPRGBK2  DS    CL10                                                             
         DS    CL3                                                              
LPRGBK3  DS    CL10                                                             
         DS    CL3                                                              
LPRGBK4  DS    CL10                                                             
         DS    CL2                                                              
LPRGPROJ DS    CL10                                                             
         SPACE 2                                                                
ROTPRGLD DSECT                                                                  
LPRG1    DS    CL10                                                             
         DS    CL2                                                              
LPRG2    DS    CL10                                                             
         DS    CL2                                                              
LPRG3    DS    CL10                                                             
         DS    CL2                                                              
LPRG4    DS    CL10                                                             
         DS    CL2                                                              
LPRG5    DS    CL10                                                             
         SPACE 2                                                                
ROTRTGLD DSECT                                                                  
LRRTG1   DS    CL11                                                             
         DS    CL1                                                              
LRRTG2   DS    CL11                                                             
         DS    CL1                                                              
LRRTG3   DS    CL11                                                             
         DS    CL1                                                              
LRRTG4   DS    CL11                                                             
         DS    CL1                                                              
LRRTG5   DS    CL10                                                             
         DS    CL2                                                              
LRCPP    DS    CL7                                                              
         EJECT                                                                  
SAVED    DSECT                     OVERLAY SAVE AREA DSECT                      
SVMINMAX DS    8F                                                               
SVPMINMX DS    2F                                                               
SVPCT    DS    H                                                                
SVNUMDT  DS    H                                                                
SVNUMHH  DS    H                                                                
SVHH     DS    XL1                                                              
SVBKS    DS    XL8                                                              
SVFLAG   DS    XL1                                                              
SVFIRST  EQU   X'80'                                                            
SVDEM    DS    XL4                                                              
SVDAYS   DS    XL1                                                              
SVTIMES  DS    XL4                                                              
SVLIDAYS DS    XL1                                                              
SVLITIME DS    XL4                                                              
SVLIPROG DS    CL16                                                             
SVLODAYS DS    XL1                                                              
SVLOTIME DS    XL4                                                              
SVLOPROG DS    CL16                                                             
SVDFLITM DS    XL4                                                              
SVDFLOTM DS    XL4                                                              
SVDTLST  DS    XL(13*5)                                                         
SVUPFILE DS    C                                                                
SVUPGRD  DS    XL8                                                              
SVUPFRBK DS    XL2                                                              
SVUPFBL  DS    XL6                                                              
SVUPINP  DS    CL32                                                             
SVUPDAY  DS    X                                                                
SVUPTIM  DS    XL4                                                              
SVUPPUT  DS    CL1                                                              
SVUPSHR  DS    CL1                                                              
SVRTGSVC DS    CL1                                                              
         DS    0F                                                               
SVDEMVAL DS    XL(4*4*13)                                                       
SVPROJ   DS    13XL4                                                            
SVLIPRGS DS    CL(4*16)                                                         
SVLOPRGS DS    CL(4*16)                                                         
SVPROGS  DS    CL(4*10*13)                                                      
SVPRJPRG DS    CL(10*13)                                                        
*                                                                               
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
       ++INCLUDE SPNWSE1D                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSE2D                                                       
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
* SPNWSNBR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSBRV                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084SPNWS34A  07/17/02'                                      
         END                                                                    
