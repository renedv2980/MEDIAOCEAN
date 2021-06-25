*          DATA SET SPNWS10    AT LEVEL 058 AS OF 02/26/07                      
*PHASE T20710C,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'NWS10 - BUYERS WORK SHEET - WORK RECAP'                         
T20710   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20710**,RA,RR=RE                                              
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
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     EXIT                                                             
         B     EXIT                                                             
         B     DISKEY                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     VALQ                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     INIT                                                             
         B     INPUT                                                            
         B     OUTPUT                                                           
         B     DRHOOK                                                           
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*=============*                                                                 
* DISPLAY KEY *                                                                 
*=============*                                                                 
         SPACE 1                                                                
DISKEY   LA    R2,INKEYT                                                        
         ZIC   R4,INKEYC                                                        
         LA    R3,FVIFLD                                                        
         XC    FVIFLD,FVIFLD                                                    
*                                                                               
DISK2    LA    R1,SAVKEYS                                                       
         SR    RF,RF                                                            
*                                                                               
DISK4    CLI   0(R1),EOT                                                        
         BNE   DISK6                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         B     DISK8                                                            
*                                                                               
DISK6    CLC   0(1,R1),0(R2)                                                    
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     DISK4                                                            
         IC    RF,1(R1)                                                         
         SHI   RF,3                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R3),2(R1)                                                    
         LA    R3,1(RF,R3)                                                      
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DISK8    LA    R2,1(R2)                                                         
         BCT   R4,DISK2                                                         
         BCTR  R3,0                                                             
         CLI   0(R3),C','                                                       
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         LA    RF,FVIFLD                                                        
         SR    R3,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         L     R1,AKEYHDR                                                       
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    R3,*+4                                                           
         MVC   L'FVIHDR(0,R1),FVIFLD                                            
         LA    R3,1(R3)                                                         
         STC   R3,FVILEN-FVIHDR(R1)                                             
         XC    BWSKY2,BWSKY2       CLEAR KEY2 FIELD                             
         OI    BWSKY2H+6,FVOXMT                                                 
*                                                                               
DISKX    B     EXIT                                                             
         EJECT                                                                  
*====================*                                                          
* VALIDATE REQUEST   *                                                          
*====================*                                                          
*                                                                               
VALQ     CLI   APPFKEY,PFK12       TEST PFKEY=12                                
         BNE   VALQ0                                                            
         TM    TWAINDS,TWAIRCP1+TWAIRCP2  YES-TEST RECAP SCREEN                 
         BZ    VALQ0                          SELECTED FROM SKED SCREEN         
         BAS   RE,RETURN           YES-RETURN TO SKED SCREEN                    
         MVI   SCPFKEY,0           PREVENT GENERAL FROM SEEING PFK12            
         B     VALQX                                                            
*                                                                               
VALQ0    OC    INOFRM,INOFRM     TEST FORMAT OPTION SET                         
         BNZ   VALQ01                                                           
         CLI   APPFKEY,PFK02     NO-PFKEY=2 IS DPT FORMAT                       
         BNE   *+14                                                             
         MVC   INOFRM,=C'PT'                                                    
         B     VALQ01                                                           
         CLI   APPFKEY,PFK03       PFKEY=3 IS STATION FORMAT                    
         BNE   *+12                                                             
         MVI   INOFRM,C'S'                                                      
         B     VALQ01                                                           
         CLI   APPFKEY,PFK04       PFKEY=4 IS ALL FORMAT                        
         BNE   *+14                                                             
         MVC   INOFRM,=C'AL'                                                    
         B     VALQ01                                                           
         CLI   APPFKEY,PFK05       PFKEY=5 IS STATION 2 FORMAT                  
         BNE   *+14                                                             
         MVC   INOFRM,=C'S2'                                                    
         B     VALQ01                                                           
*                                                                               
VALQ01   LA    R2,IOKEY                                                         
         USING BWHKEY,R2                                                        
         LA    R3,APRECKEY                                                      
         USING BWDKEY,R3                                                        
*                                                                               
         MVI   INWHEN,X'01'       REPORT IS NOW                                 
         XC    BWHKEY,BWHKEY      SET UP HEADER KEY                             
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         GOTO1 VSCANNER,APPARM,AKEYHDR,AIOAREA1                                 
         NI    LFLAG,X'FF'-LVALKEY2   INDICATE KEY1                             
         NI    LFLAG,X'FF'-LAFGSFRM   RESET AFG+SPECIAL FORMAT FLAG             
         LA    R8,1               ERROR INDEX(FIELD INTO THE KEY)               
         MVI   BDPT,0                                                           
         MVI   BSLN,0                                                           
         MVI   FSTA,0                                                           
         XC    COMDPLST,COMDPLST                                                
         XC    DPTSUBS,DPTSUBS    CLEAR -BECAUSE SKED USES THIS                 
         XC    SVCMP2,SVCMP2      RANGE OF CAMPAIGNS                            
*                                                                               
VALQ1    SR    R0,R0                                                            
         ICM   R0,1,4(R1)         # OF KEY FIELDS ENTERED                       
         BZ    VALQ98                                                           
         L     R4,AIOAREA1        POINT TO REQUEST                              
*                                                                               
VALQ2    STC   R8,FVINDX                                                        
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)         LENGTH OF FIELD                               
         BZ    VALQ98                                                           
         CLI   FVINDX,3           FOR CMP - SECOND HALF ALLOWED                 
         BE    VALQ3                                                            
         CLI   1(R4),0            ANY SECOND HALF?                              
         BNE   VALQ94             YES - ERROR                                   
VALQ3    BAS   RE,VALQSETF        MOVES DATA TO APWORK                          
         BE    VALQ4              NO PROBLEMS, CONTINUE                         
         CLI   FVINDX,3           HAD A != CONDITION SOMEWHERE, CHECK           
         BE    VALQ99             FROM CMPSETUP SUBROUTINE, ERROR OUT           
*                                                                               
VALQ4    CLI   FVINDX,1           FIRST FIELD?                                  
         BNE   VALBYR             NO - TRY NEXT FIELD                           
         GOTO1 AVALMED,APWORK     YES VALIDATE MEDIA                            
         BNE   VALQ99                                                           
         MVC   BWHKAGMD,BAGYMD                                                  
         B     VALQ32             GET NEXT KEY VALUE                            
*                                                                               
VALBYR   CLI   FVINDX,2           SECOND FIELD?                                 
         BNE   VALCAM             NO - TRY NEXT FIELD                           
         GOTO1 AVALBYR,APWORK                                                   
         BNE   VALQ99             ERROR TO SCREEN                               
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR       MOVE INTO REC                                 
         OC    BYRPW,BYRPW        ANY SPECIAL PASSWORD?                         
         BZ    VALQ32             NO - TRY NEXT KEY VALUE                       
         GOTO1 AVALPWD            YES - VALIDATE PASSWORD                       
         BNE   VALQX              INCORRECT PASSWORD                            
         B     VALQ32                                                           
*                                                                               
VALCAM   CLI   FVINDX,3           THIRD FIELD?                                  
         BNE   VALMOS              - NAH - TRY NEXT FIELD                       
*                                                                               
         LA    R9,CMPTAB           SAVE OFF THE ADDRESS OF R9                   
         ST    R9,SAVER9           NEED FOR 'INPUT'                             
*                                                                               
         CLI   2(R9),X'FF'         RANGE OF CAMPAIGNS?                          
         BE    VALCAM3              - NOPE, GET OUTTA HERE                      
         MVI   INOSTDTD,0          STDATE OPTION NOT ALLOWED W/ RANGE           
         XC    INOSTDTE,INOSTDTE                                                
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,CMPTAB        JUST NEED THE FIRST NUMBER IN TABLE           
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  CMP,APDUB                                                        
         OC    INOFRM,INOFRM      OPTIONS NOT ALLOWED WITH CAMP RANGE           
         BZ    VALCAM2                                                          
         CLI   INOFRM,C'W'           WEEKLY GVP FOR DAILY CAMPAIGN?             
         BE    VALCAM2               YES, STILL GVP BUT INOFRM SET              
******** CLI   CUDMED,C'C'                                                      
******** BNE   VALQIVC2           US CAMPAIGN RANGE NOT ALLOWED OPTION          
         CLI   INOFRM,C'G'        CAN CAMP RANGE ALLOWED CERTAIN ONES           
         BE    VALQIVC2                                                         
         CLI   INOFRM,C'D'                                                      
         BE    VALQIVC2                                                         
         CLI   INOFRM,C'Y'                                                      
         BE    VALQIVC2                                                         
*                                                                               
VALCAM2  XC    APWORK,APWORK                                                    
         MVC   APWORK+8(L'CMP),CMP                                              
         LA    R1,L'CMP                                                         
         STC   R1,APWORK+5        LENGTH OF CMP                                 
         LA    R1,8(R1)           +HEADER LENGTH                                
         STC   R1,APWORK                                                        
*                                                                               
VALCAM3  GOTO1 AVALCAM,APWORK     YES - VALIDATE CAMPAIGN                       
         BNE   VALQ99             ERROR TO SCREEN                               
         MVC   BWHKCAM,BCAM       MOVE IN RECORD                                
********                                                                        
* ONLY FOR NO FORMAT                                                            
********                                                                        
         CLI   CMPNWKS,14          SCROLL ONLY IF MORE THAN 14 WKS              
         BNH   VALCAM3X                                                         
         CLI   INOFRM,C'W'             WEEKLY IS LIKE NO FORMAT                 
         BE    *+14                                                             
         OC    INOFRM,INOFRM                      NO FORMAT                     
         BNZ   VALCAM3X                                                         
         OC    INOSTDTE,INOSTDTE                  NO STDATE OPTION              
         BNZ   VALCAM3X                                                         
*                                                                               
         ZIC   RE,RCPDSTDT                                                      
         CLI   APPFKEY,PFK08       SCROLLING INTO FUTURE?                       
         BNE   *+12                                                             
         LA    RF,19                                                            
         B     VALCAM3B                                                         
*                                                                               
         CLI   APPFKEY,PFK07       SCROLLING INTO PAST?                         
         BNE   VALCAM3X                                                         
         LHI   RF,-19                                                           
*                                                                               
VALCAM3B AR    RE,RF                                                            
         BNM   VALCAM3D                                                         
         MVI   RCPDSTDT,0                                                       
         B     VALCAM3X                                                         
*                                                                               
VALCAM3D CLM   RE,1,CMPNWKS                                                     
         BNL   VALCAM3X                                                         
         STC   RE,RCPDSTDT                                                      
*                                                                               
VALCAM3X BAS   RE,GETCPE          GET CLIENT/PROD/EST DETAILS                   
         BNE   VALQX                                                            
***  AFG  ***                                                                   
         TM    INOIND,INOIALG      ARE WE DOING ALL GOALS?                      
         BZ    VALCAM3Z             - NOPE, NORMAL                              
*                                                                               
         OC    INOFRM,INOFRM       TEST FORMAT OPTION SET                       
         BZ    VALCAM3Z                                                         
         CLC   =C'PT',INOFRM       DAYPART?                                     
         BE    VALCAM3Y             - YUP!                                      
         CLC   =C'AL',INOFRM       ALL?                                         
         BE    VALCAM3Y             - YUP!                                      
         CLC   =C'A2',INOFRM       ALL 2?                                       
         BNE   VALCAM3Z            NONE OF THE ABOVE, CONTINUE                  
*                                                                               
VALCAM3Y BRAS  RE,DPTSETUP         LETS SETUP OUR COMPLETE DAYPART LIST         
*                                  ...SETS UP SPOT LENGTH LIST AS WELL          
         NI    INOIND,X'FF'-INOIALG   TRN OFF FLAG SO 00 TREAT NORMALLY         
         OI    LFLAG,LAFGSFRM      TURN ON LOCAL AFG+SPECIAL FORMAT FLG         
***  AFG  ***                                                                   
VALCAM3Z XC    LDEMHLD,LDEMHLD    MOVE EST INFO TO LDEMHLD                      
         LA    RE,LDEMHLD                                                       
         LA    RF,ESTDEMS+1                                                     
         LA    R1,4                                                             
         OC    INODEM,INODEM      USE OVERRIDE DEMOS IF GIVEN                   
         BZ    VALCAM4                                                          
         ZIC   R1,INODEM          NUMBER OF OVERRIDE DEMOS                      
         LA    RF,INODEM+2                                                      
VALCAM4  MVC   0(2,RE),0(RF)                                                    
         LA    RE,6(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R1,VALCAM4                                                       
*                                                                               
         XC    DBLOCK,DBLOCK       GET DEMO NAMES                               
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         MVC   DBCOMFCS,ACOM                                                    
         LA    RE,ESTDEMS                                                       
         MVI   APBYTE,4                                                         
         ICM   RE,8,APBYTE        GET INTO HIGH ORDER BYTE                      
         ST    RE,APPARM                                                        
         OC    INODEM,INODEM                                                    
         BZ    VALCAM6                                                          
         LA    RE,INODEM+1        USE OVERRIDE VALUES                           
         MVC   APBYTE,INODEM                                                    
         ICM   RE,8,APBYTE                                                      
         ST    RE,APPARM                                                        
VALCAM6  LA    RE,ESTUSRNM                                                      
         ST    RE,APPARM+12                                                     
         XC    COMDNAMS,COMDNAMS                                                
         GOTO1 VDEMOCON,APPARM,,(2,COMDNAMS),(C'S',DBLOCK)                      
         B     VALQ32                                                           
*                                                                               
VALMOS   CLI   FVINDX,4           FOURTH FIELD?                                 
         BNE   VALDPL             NO - TRY NEXT FIELD                           
         TM    2(R4),X'80'        IF FIELD IS NUMERIC                           
         BZ    VALSTA                                                           
VALMKT   GOTO1 AVALMKT,APWORK     YES-MUST BE MARKET                            
         BNE   VALQ99             NOT VALID MARKET                              
         MVI   FSTA,0             MARKET REQUEST                                
         B     VALQ28             GO VALIDATE IT                                
*                                                                               
VALSTA   GOTO1 AVALSTA,APWORK     VALIDATE STATION                              
         BNE   VALQ99                                                           
         MVI   FSTA,1             STATION ENTERED                               
*                                                                               
VALQ28   MVC   BWHKMKT,BMKT                                                     
         B     VALQ32             GET NEXT KEY FIELD                            
*                                                                               
VALDPL   CLI   FVINDX,5            FIFTH FIELD?                                 
         BNE   VALQ94              IF NOT - ERROR                               
         GOTO1 AVALDPL,APWORK     YES - GO VALIDATE DAYPART- LENGTH             
         BNE   VALQ99                                                           
*                                                                               
         CLI   CMPDPOPT,C'M'      TEST SUBDPTS SCHEDULED UNDER MASTER           
         BNE   VALDPL2                                                          
         CLI   DPTTYPE,C'S'       YES -TEST SUBDAYPART                          
         BE    VALQ96             YES - ERROR                                   
*                                                                               
VALDPL2  CLI   INOFRM,C'A'        ALL DAYPARTS?                                 
         BNE   VALQ32             NO - GET NEXT KEY VALUE                       
*                                                                               
         CLI   DPTTYPE,C'R'       REGULAR DAYPART?                              
         BNE   VALDPL4                                                          
         MVC   SUBS(1),BDPT       SAVE DAYPART                                  
         MVC   COMDPLST(1),SUBS   ADD IT TO TABLE                               
         B     VALQ32                                                           
*                                                                               
VALDPL4  CLI   DPTTYPE,C'M'       MASTER DAYPART?                               
         BNE   VALDPL6                                                          
         MVC   SUBS(1),BDPT       MOVE IN MASTER DAYPART                        
         CLI   CMPDPOPT,C' '                                                    
         BNH   VALDPL8                                                          
         CLI   CMPDPOPT,C'N'                                                    
         BE    VALDPL8                                                          
         MVC   SUBS+1(L'DPTSUBS),DPTSUBS  SAVE SUBDAYPARTS                      
         B     VALDPL8            ADD MASTER & SUBS TO DPT TABLE                
*                                                                               
VALDPL6  MVC   TMPDPT,BDPT                                                      
*                                                                               
         CLI   BDPT,0              ANY DPT REQUESTED BY THE USER?               
         BE    VALQ32              NONE, NO SUBS TO GET                         
*                                                                               
         BAS   RE,GETSUBS         GET OTHER SUBDAYPARTS                         
         BNE   VALQ99                                                           
*                                                                               
VALDPL8  LA    R1,SUBS            ADD MASTER & SUBS TO DPT TABLE                
         LA    RF,L'SUBS                                                        
VALDPL9  CLI   0(R1),0                                                          
         BE    VALQ32                                                           
         MVC   TMPDPT,0(R1)                                                     
         BRAS  RE,ADDPT           ADD TO TABLE                                  
         LA    R1,1(R1)                                                         
         BCT   RF,VALDPL9                                                       
         B     VALQ32             GET NEXT KEY VALUE                            
*                                                                               
VALQ32   LA    R4,32(R4)          NEXT KEY VALUE                                
         LA    R8,1(R8)           UPDATE FIELD NUMBER                           
         BCT   R0,VALQ2                                                         
*                                                                               
         LA    R8,1               CHECK FOR ALL KEY FIELDS PRESENT              
         OC    BWHKAGMD,BWHKAGMD                                                
         BZ    VALQ98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKBYR,BWHKBYR                                                  
         BZ    VALQ98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKCAM,BWHKCAM                                                  
         BZ    VALQ98                                                           
         LA    R8,1(R8)                                                         
         OC    BWHKMKT,BWHKMKT    MARKET IN KEY?                                
         BNZ   VALQ40             YES - GO READ RECORD                          
         TM    LFLAG,LVALKEY2     TEST VALIDATING 2ND KEY FIELD                 
         BO    VALQ98             YES - MUST HAVE MARKET - ERROR                
         OI    LFLAG,LVALKEY2     VALIDATE FOR 2ND KEY FIELD                    
         MVI   FVMINL,1           FOR MARKET AND DPT/LEN                        
         LA    R1,BWSKY2H         POINT TO KEY2                                 
         GOTO1 AFVAL                                                            
         BNE   VALQX                                                            
         GOTO1 VSCANNER,APPARM,(C'C',FVIFLD),(2,AIOAREA1)                       
         B     VALQ1                                                            
*                                                                               
VALQ40   MVC   HDRKEY,BWHKEY      SAVE HEADER KEY                               
         BAS   RE,BLDDTL          BUILD DTL KEY FOR READREC                     
         BAS   RE,SETOPTS                                                       
         B     VALQX                                                            
         DROP  R2,R3                                                            
         SPACE                                                                  
* MOVE APPROPRIATE DATA TO APWORK                                               
VALQSETF ST    RE,SAVERE           SAVE OFF RE                                  
         XC    APWORK,APWORK                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   APWORK+L'FVIHDR(0),12(R4)                                        
         LA    RF,1(RF)                                                         
         STC   RF,FVILEN-FVIHDR+APWORK                                          
         LA    RF,L'FVIHDR(RF)                                                  
         STC   RF,APWORK                                                        
         CLI   FVINDX,3                                                         
         BNE   VALQSETX                                                         
         BAS   RE,CMPSETUP         SETS UP CMPTAB - CONDITION CODE SET          
VALQSETX L     RE,SAVERE           RESTORE RE                                   
         BR    RE                                                               
         EJECT                                                                  
*-------------------------------------------------------------*                 
* CMPSETUP - SETUPS UP THE CMPTAB            MHC  06/13/03    *                 
*-------------------------------------------------------------*                 
CMPSETUP NTR1                                                                   
         LA    R3,CMPTAB                                                        
         MVI   CMPTAB+L'CMPTAB-1,X'FF'   MOVE X'FF' INTO LAST BYTE              
*                                                                               
         MVC   FAKEFLD(16),12(R4)   WE'RE USING A FAKE FIELD                    
         MVC   FAKEFLDH+5(1),0(R4)   LENGTH OF INPUT                            
         GOTO1 VSCANNER,APPARM,FAKEFLDH,AIOAREA3,C',=/-'                        
         L     R2,AIOAREA3                                                      
         USING SCANBLKD,R2                                                      
*                                                                               
CMP10    CLI   0(R3),X'FF'         DID WE HIT END OF TABLE YET?                 
         BNE   CMP13                - NOPE, JUST CONTINUE NORMALLY              
         MVC   FVMSGNO,=AL2(FVMORE18)   MORE THAN 18 CMPS IN RANGE              
         B     CMPNO               LET'S GET OUTTA HERE                         
*                                                                               
CMP13    CLI   SC1STLEN,0          IS THERE ANYTHING IN THE FIRST FIELD         
         BE    CMPX                 - NAH THERE ISN'T ANYTHING                  
         CLI   SC2NDLEN,0          DO WE HAVE A SECOND FIELD?                   
         BE    CMP50                - NO WE DON'T (NOT A RANGE)                 
         CLC   SC2NDNUM,SC1STNUM   IS THE FIRST NUMBER GREATER                  
         BH    CMP15                - YUP, IT'S GOOD TO GO                      
         MVC   FVMSGNO,=AL2(FVCMPRNG)   INVALID CAMPAIGN RANGE                  
         B     CMPNO               ERROR EXIT                                   
*                                                                               
CMP15    L     R6,SC1STNUM         STORE THE NUMBER IN R6 AND THEN BUMP         
CMP20    CLI   0(R3),X'FF'         DID WE HIT END OF TABLE YET?                 
         BNE   CMP30                - NOPE, JUST CONTINUE NORMALLY              
         MVC   FVMSGNO,=AL2(FVMORE18)   MORE THAN 18 CMPS IN RANGE              
         B     CMPNO               LET'S GET OUTTA HERE                         
*                                                                               
CMP30    STCM  R6,3,0(R3)                                                       
         CLM   R6,15,SC2NDNUM      DID WE GET UP TO SC2NDNUM VALUE?             
         BNL   CMPNEXT              - YUP WE DID                                
         LA    R3,2(R3)                                                         
         LA    R6,1(R6)                                                         
         B     CMP20                                                            
*                                                                               
CMP50    MVC   0(2,R3),SC1STNUM+2  JUST MOVE THE SINGLE NUMBER                  
CMPNEXT  LA    R2,32(R2)           BUMP TO THE NEXT ENTRY                       
         LA    R3,2(R3)                                                         
         B     CMP10                                                            
*                                                                               
CMPNO    CR    RB,RD               SET CONDITION CODE TO "NOT EQUAL"            
CMPX     MVI   0(R3),X'FF'         TO SIGNAL AS END OF TABLE                    
         B     EXIT                                                             
*-------------------------------------------------------------*                 
* GETCPE  - GET CLIENT/PROD & ESTIMATE DETAILS FOR A CAMPAIGN *                 
*-------------------------------------------------------------*                 
*                                                                               
GETCPE   NTR1                                                                   
         GOTO1 AGETCLT,CMPCLTC    (GET CLIENT)                                  
         BNE   GETCPEN                                                          
         XC    LPRDNM1,LPRDNM1                                                  
         XC    LPRDNM2,LPRDNM2                                                  
         CLI   CMPPRD1,0          TEST FOR PIGGBACKS                            
         BE    GETCPE2                                                          
         GOTO1 AGETPRD,CMPPRD1    GET PIGGYBACK PRD 1                           
         MVC   LPRDNM1,PRDNM                                                    
         CLI   CMPPRD2,0                                                        
         BE    GETCPE2                                                          
         GOTO1 AGETPRD,CMPPRD2    GET PIGGYBACK PRD 2                           
         MVC   LPRDNM2,PRDNM                                                    
*                                                                               
GETCPE2  GOTO1 AGETPRD,CMPPRDN    (GET PRODUCT)                                 
         BNE   GETCPEN                                                          
         GOTO1 AGETEST,CMPESTN    GET CAMPAIGN ESTIMATE DETAILS                 
         BNE   GETCPEN                                                          
         CR    RB,RB                                                            
         B     GETCPEX                                                          
*                                                                               
GETCPEN  LTR   RB,RB                                                            
GETCPEX  B     EXIT                                                             
         EJECT                                                                  
*------------------------------*                                                
* BLDDTL  - BUILDS DETAIL KEY  *                                                
*------------------------------*                                                
*                                                                               
BLDDTL   NTR1                                                                   
         MVC   IOKEY,HDRKEY       READ HEADER POINTER                           
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   BLDDN                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BE    BLDD45                                                           
         OI    TWAFLAG,TWANOHDR   NO                                            
         LA    R1,BWSKEYH         SET CURSOR TO KEY FIELD                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     BLDDN                                                            
*                                                                               
BLDD45   MVC   HDRDA,IODA         SAVE HEADER D/A                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO1   GET HEADER RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         USING BWHKEY,R2                                                        
         MVC   BCMSEQ,BWHKSEQ     SAVE SEQUENCE NUMBER                          
         SR    RE,RE                                                            
         MVI   BSTACD,0                                                         
         CLI   FSTA,1             TEST STATION REQUESTED?                       
         BNE   BLDD50                                                           
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
         SR    RE,RE                                                            
*                                                                               
BLDD46   CLI   0(R4),0                                                          
         BE    BLDD48                                                           
         CLI   0(R4),BWHELCDQ                                                   
         BNE   BLDD47                                                           
         USING BWHEL,R4                                                         
         IC    RE,BWHSEQ                                                        
         CLC   QSTA,BWHSTA                                                      
         BNE   BLDD47                                                           
         STC   RE,BSTACD          SET STATION SEQ NO                            
         B     BLDD50                                                           
*                                                                               
BLDD47   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BLDD46                                                           
*                                                                               
BLDD48   OI    TWAFLAG,TWANOSTA   NO STATION FOUND                              
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         B     BLDDN                                                            
*                                                                               
BLDD50   LA    R3,DTLKEY          BUILD DETAIL KEY                              
         XC    DTLKEY,DTLKEY                                                    
         USING BWDKEY,R3                                                        
         XC    BWDKEY(13),BWDKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    X'0D'                                        
         MVI   BWDKSUB,BWDKSUBQ    X'68'                                        
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ     SEQUENCE NUMBER FROM HEADER KEY              
         MVI   BWDKELCD,BWDELCDQ                                                
         STC   RE,BWDKELST        STATION CODE                                  
         CR    RB,RB                                                            
         B     BLDDX                                                            
BLDDN    LTR   RB,RB                                                            
BLDDX    B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*-------------------------------------------------------------*                 
* SETOPTS - SETS DETL,GVPFORM, PERDISPS, PERDISPE             *                 
*-------------------------------------------------------------*                 
*                                                                               
SETOPTS  NTR1                                                                   
         MVI   DETL,C'W'         WEEKLY FORMAT                                  
         OC    INOFRM,INOFRM      ANY OPTION?                                   
         BZ    SETOPT5                                                          
         CLI   INOFRM,C'W'                                                      
         BE    SETOPT5            GVP BY WEEK IF DAILY CAMPAIGN                 
*                                                                               
         CLI   INOFRM,C'G'                                                      
         BE    SETOPT5            GVP BY WEEK                                   
         CLI   INOFRM,C'D'                                                      
         BE    SETOPT5            DEM BY WEEK                                   
         CLI   INOFRM,C'Y'        DAILY BY WEEK                                 
         BE    SETOPT5                                                          
         MVI   DETL,C'S'          NOT WEEKLY FORMAT (STATION MAYBE?)            
         CLI   INOFRM,C'S'                                                      
         BE    SETOPT5            STATION FORMAT                                
         MVI   DETL,C'D'          ONLY LEAVES DAYPART                           
*                                                                               
SETOPT5  MVI   GVPFORM,C'N'       CPP/CPM FORMAT                                
         CLI   INOFRM,C'D'                                                      
         BE    SETOPT8                                                          
         CLI   INOFRM+1,C'D'                                                    
         BE    SETOPT8                                                          
         MVI   GVPFORM,C'Y'       GVP FORMAT                                    
*                                                                               
SETOPT8  BRAS  RE,SETPER          CALC PERIOD START & END                       
         B     EXIT                                                             
         EJECT                                                                  
***********                                                                     
* ERRORS  *                                                                     
***********                                                                     
VALQ94   MVC   FVMSGNO,=AL2(FVFNOTV)      INVALID INPUT FIELD                   
         B     VALQ99                                                           
*                                                                               
VALQ95   MVC   FVMSGNO,=AL2(FVMKTSTA) MKT INVALID-MUST ENTER STATION            
         B     VALQ99                                                           
*                                                                               
VALQ96   MVC   FVMSGNO,=AL2(FVISDPT)    INVALID SUBDAYPART                      
         B     VALQ99                                                           
*                                                                               
VALQ97   MVC   FVMSGNO,=AL2(FVSTADPT)                                           
         B     VALQ99                                                           
*                                                                               
VALQ98   MVC   FVMSGNO,=AL2(FVFNONE)   KEY FIELD MISSING                        
         B     VALQ99                                                           
*                                                                               
VALQINVC MVC   FVMSGNO,=AL2(FVCMPRNG)  INVALID CAMPAIGN RANGE                   
VALQ99   LA    R1,BWSKEYH         KEY ERROR EXIT                                
         TM    LFLAG,LVALKEY2     TEST VALIDATING SECOND KEY FIELD              
         BZ    *+12                                                             
         LA    R1,BWSKY2H         POINT TO KEY2                                 
         SHI   R8,3               RESET INDEX                                   
         ST    R1,FVADDR                                                        
         STC   R8,FVINDX                                                        
VALQX    B     EXIT                                                             
*                                                                               
VALQIVC2 MVC   FVMSGNO,=AL2(FVOPTRNG)                                           
         B     VALQX                                                            
         EJECT                                                                  
* ==========================*                                                   
* RETURN TO SCHEDULE SCREEN *                                                   
* ==========================*                                                   
         SPACE 1                                                                
RETURN   NTR1  ,                                                                
         L     R4,ATIA                                                          
         GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(4,0),(R4)                           
         LA    R0,BWSKEYH                                                       
         LA    RE,BWSKEYH-TWAD(R4)                                              
         DROP  R6                                                               
         LH    R1,=Y(SAVAREA-BWSKEYH)                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R1,BWSKEYH          TRANSMIT THE SCREEN                          
         LA    RF,SAVAREA-3                                                     
         SR    RE,RE                                                            
         OI    6(R1),FVOXMT                                                     
         ICM   RE,1,0(R1)                                                       
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                                                             
         MVI   1(R1),1                                                          
         MVI   2(R1),1                                                          
         MVI   APMODE,APMRET       PASS RETURN MODE                             
         MVI   APPARM,RECWRK       RECORD=WORK                                  
         MVI   APPARM+1,ACTSKD     ACTION=SKED                                  
         TM    TWAINDS,TWAIRCP1                                                 
         BO    *+8                                                              
         MVI   APPARM+1,ACTSSK     OR ACTION=SSKED                              
         NI    TWAINDS,255-(TWAIRCP1+TWAIRCP2)                                  
         ICM   R1,15,AINP                                                       
         BZ    EXIT                                                             
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSCRN                                                
         MVI   TIOBCNT,X'FB'                                                    
         CLI   APPARM+1,ACTSSK                                                  
         BNE   *+8                                                              
         MVI   TIOBCNT,X'FA'                                                    
         MVI   TIOBCNT+1,0                                                      
         DROP  R1                                                               
         B     EXIT                                                             
         EJECT                                                                  
* =============================*                                                
* INIT - INITIALIZATION MODE   *                                                
* =============================*                                                
*                                                                               
INIT     MVI   FSTINPUT,C'Y'      FIRST TIME FOR INPUT SWITCH                   
         MVI   ENDSW,0            ENDSW=X'FF' FINISHED INPUT RECS               
         MVI   READRC,C'D'        D=DETAIL RECORDS, G=GOAL RECORDS              
*                                                                               
         XC    DISPDPT,DISPDPT                                                  
         XC    DISPSLN,DISPSLN                                                  
         XC    TOTGDOL,TOTGDOL                                                  
         XC    TOTGPNT,TOTGPNT                                                  
         XC    SUBGDOL,SUBGDOL                                                  
         XC    SUBGPNT,SUBGPNT                                                  
*                                                                               
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         GOTO1 VCOLY,APPARM,(X'30',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GLASYSDR,0(R1)     SAVE ADDRESS OF SYSTEM DRIVER                 
*                                                                               
         GOTO1 VCOLY,APPARM,(X'51',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GLAPROG,0(R1)      SAVE ADDRESS OF DPG PROGRAM                   
*                                                                               
         LA    RF,RECL4H          FIRST HEADER FIELD(TWA)                       
         ST    RF,APPARM          NEEDED FOR CALL TO DROOL                      
         LA    RF,SPARE           GIVE DROOL SPACE FOR                          
         ST    RF,GLADTAB         DRIVE TABLE                                   
         LH    RF,=Y(SPAREX-SPARE)                                              
         ST    RF,GLSIZE                                                        
*                                 SET GLOBAL OPTIONS                            
         MVI   GLOPTS,C'Y'        GVP FORMAT                                    
*                                                                               
         OI    GLINDS,GLISDONT    SUPPRESS SPACING FOR REJECT LNS               
*                                                                               
         CLI   GVPFORM,C'Y'                                                     
         BE    INIT10                                                           
         MVI   GLOPTS,C'N'        CPP/CPM FORMAT                                
INIT10   MVI   GLOPTS+1,C'W'      WEEK DETAIL DEFAULT                           
         CLI   DETL,C'W'                                                        
         BE    INIT20                                                           
         MVI   GLOPTS+1,C'S'      STATION DETAIL                                
         CLI   DETL,C'S'                                                        
         BE    INIT20             YES STATION                                   
         MVI   GLOPTS+1,C'D'      DPT-LEN DETAIL                                
*                                                                               
INIT20   MVI   GLOPTS+2,C'Y'      STATION WAS ENTERED IN KEY                    
         CLI   FSTA,1                                                           
         BE    *+8                                                              
         MVI   GLOPTS+2,C'N'                                                    
         MVI   GLOPTS+3,C'N'      DON'T USE WEEKLY FIGURES                      
         CLI   DETL,C'W'          IF NOT WEEKLY FORMAT                          
         BNE   INIT25             OR IF WEEKLY FORMAT                           
         CLI   FSTA,1             BUT STATION IN KEY                            
         BE    INIT25                                                           
         MVI   GLOPTS+3,C'Y'                                                    
INIT25   MVI   GLOPTS+4,C'N'                                                    
         CLI   INOFRM,C'A'                                                      
         BNE   *+8                                                              
         MVI   GLOPTS+4,C'S'      ALL DAYPARTS                                  
         MVI   GLOPTS+5,C'N'                                                    
         CLC   =C'S2',INOFRM                                                    
         BNE   *+8                                                              
         MVI   GLOPTS+5,C'2'      SECOND STATION FORMAT                         
*                                                                               
         MVI   GLOPTS+6,C'N'                                                    
         CLI   CMPTAB+2,X'FF'     WE'RE ONLY DOING ONE CAMPAIGN?                
         BE    INIT30              - YUP!                                       
*                                                                               
         CLI   INOFRM,C'W'                                                      
         BE    *+14                                                             
         OC    INOFRM,INOFRM                                                    
         BNZ   INIT30                                                           
         MVI   GLOPTS+6,C'Y'      CAMPAIGN TOTAL DISPLAY                        
*                                                                               
INIT30   MVI   GLOPTS+7,C'N'                                                    
         CLC   INOFRM,=C'A2'      IF DAYPART =ALL PERCENTAGE                    
         BNE   *+8                                                              
         MVI   GLOPTS+7,C'Y'                                                    
*                                                                               
INIT40   MVI   GLFHEADL,10                                                      
         MVI   GLLHEADL,13                                                      
         MVI   GLBOXOPT,C'N'                                                    
         MVI   GLDETHED,C'N'                                                    
         MVI   GLTWORKD,GLTSPBWS  WORKD PROVIDING                               
         ST    R7,GLAWORKD        GIVE ADDRESS OF WORKING STORAGE               
*                                                                               
INTDRIVX L     R3,AREP                                                          
         USING REPD,R3                                                          
         MVI   REPMAXL,20          MAX NUMBER OF LINES PER PAGE NOW 20!         
         OI    REPIND1,REPIONL     ONLINE REPORT                                
         DROP  R3                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*================================*                                              
* INPUT - INPUT FOR DRIVER/DROOL *                                              
*================================*                                              
*                                                                               
INPUT    GOTO1 VGETFACT,APPARM,0 GET COUNT FORM GETFACT                         
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
*                                                                               
         L     R9,SAVER9                                                        
*                                                                               
         ICM   R3,3,FATMAXIO      MAXIMUM ALLOWABLE IOS                         
         MHI   R3,95                                                            
         D     R2,=F'100'         95 PERCENT OF MAX IOS IN R3                   
         CLM   R3,3,FATIOCNT      TEST RUNNING OUT OF IOS                       
         BH    INPUT2                                                           
         MVC   FVMSGNO,=AL2(FVCMPLRG)                                           
         B     INPUTX                                                           
*                                                                               
INPUT2   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
INPUT5   CLI   READRC,C'D'                                                      
         BNE   INPUT20                                                          
         BAS   RE,READREC         READ DETAIL RECORDS                           
         CLI   ENDSW,X'FF'                                                      
         BNE   INPUTX             NO MORE RECORDS                               
         CLI   GVPFORM,C'N'       IF CPP/CPM FORMAT DON'T                       
         BE    INPUT30            NEED TO READ GOALS                            
         MVI   READRC,C'G'        READING GOALS                                 
         MVI   FSTGIN,C'Y'        FIRST TIME READING GOALS                      
         MVI   ENDSW,0            RESET END SWITCH                              
         MVI   DYSADD,0           IF DAILY -USE START OF WEEK                   
         MVI   WEEKNO,0           START AT FIRST WEEK                           
*                                                                               
INPUT20  BAS   RE,GOALS                                                         
         CLI   ENDSW,X'FF'                                                      
         BNE   INPUTX                                                           
INPUT30  CLI   CMPTAB+2,X'FF'      ONLY ONE CAMPAIGN NEEDED?                    
         BE    INPUT50              - YUP, THAT'S IT!                           
*                                                                               
         LA    R9,2(R9)            BUMP CMPTAB                                  
         CLI   0(R9),X'FF'         IS THE TABLE OVER?                           
         BE    INPUT50              - YUP, DONE WITH ALL CAMPAIGNS              
         XR    R1,R1                                                            
         ICM   R1,3,0(R9)          CURRENT CAMPAIGN                             
         BZ    INPUT50              - SHOULDN'T BE                              
         ST    R9,SAVER9           SAVE IT OFF AGAIN                            
*                                                                               
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  CMP,APDUB                                                        
         XC    APWORK,APWORK                                                    
         MVC   APWORK+8(L'CMP),CMP                                              
         LA    R1,L'CMP                                                         
         STC   R1,APWORK+5        LENGTH OF CMP                                 
         LA    R1,8(R1)           +HEADER LENGTH                                
         STC   R1,APWORK                                                        
*                                                                               
         BAS   RE,CHKCMP          CHECK NEW CAMPAIGN & SETS HDRKEY              
         BNE   INPUTX                                                           
*                                                                               
INPUT40  BAS   RE,BLDDTL          SET KEY FOR READREC                           
         BE    INPUT45                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     INPUT30                                                          
INPUT45  MVI   FSTINPUT,C'Y'      RESET SOME INDICATORS                         
         MVI   ENDSW,0                                                          
         MVI   READRC,C'D'                                                      
         XC    DISPDPT,DISPDPT                                                  
         XC    DISPSLN,DISPSLN                                                  
***  AFG  ***                                                                   
         TM    LFLAG,LAFGSFRM      WE DOING AFG+SPECIAL FORMAT?                 
         BO    INPUT5                                                           
***  AFG  ***                                                                   
         XC    COMSLLST,COMSLLST                                                
         XC    COMDPLST,COMDPLST                                                
         B     INPUT5                                                           
*                                                                               
INPUT50  MVI   APMODE,APMDREND    NO MORE RECORDS FOR INPUT                     
INPUTX   L     R3,AREP                                                          
         USING REPD,R3                                                          
         MVI   REPMAXL,20          MAX NUMBER OF LINES PER PAGE NOW 20!         
         OI    REPIND1,REPIONL     ONLINE REPORT                                
         DROP  R3                                                               
         B     EXIT                                                             
         EJECT                                                                  
*=========================================*                                     
* CHKCMP  - CHECK NEXT CAMP IN RANGE      *                                     
*=========================================*                                     
*                                                                               
CHKCMP   NTR1                                                                   
         MVC   MYSAVBV,BMKT        SAVE VALUES                                  
         MVC   MYSAVQV,QMKT                                                     
         GOTO1 AVALCAM,APWORK     YES - VALIDATE CAMPAIGN                       
         BNE   CHKCMPE1                                                         
         LA    R1,HDRKEY          SET NEW HEADER KEY                            
         LA    R1,BWHKCAM-BWHKEY(R1)                                            
         MVC   0(L'BWHKCAM,R1),BCAM                                             
         BAS   RE,GETCPE          GET CLIENT/PRODUCT/ESTIMATE DETAILS           
         BNE   CHKCMPE1                                                         
         MVC   BMKT(L'MYSAVBV),MYSAVBV  RESTORE VALS CLEARED BY GETCPE          
         MVC   QMKT(L'MYSAVQV),MYSAVQV                                          
         BRAS  RE,SETPER          RE-SET PERIODS                                
*                                                                               
         LA    R1,4               CHECK 4 DEMOS                                 
         CLI   INOFRM,C'D'                                                      
         BE    CHKCMP2                                                          
         CLI   INOFRM+1,C'D'      IF DEMO DISPLAY                               
         BE    CHKCMP2                                                          
         LA    R1,2               CHECK 2 DEMOS                                 
         CLC   INOFRM,=C'S2'      SECOND STATION DISPLAY                        
         BE    CHKCMP2                                                          
         LA    R1,1                                                             
*                                                                               
CHKCMP2  OC    INODEM,INODEM      OVERRIDE DEMO'S GIVEN                         
         BZ    CHKCMP3                                                          
         ZIC   R0,INODEM          MAKE SURE ENOUGH GIVEN                        
         CR    R0,R1                                                            
         BL    CHKCMPE2           NOT ENOUGH                                    
         B     CHKCMPY            GOOD                                          
*                                                                               
CHKCMP3  LA    RE,LDEMHLD         PT TO 1ST CAMPAIGNS INFO                      
         LA    RF,ESTDEMS+1       COMPARE TO NEW CAMPAIGN INFO                  
CHKCMP4  CLC   0(2,RE),0(RF)                                                    
         BNE   CHKCMPE2                                                         
         LA    RE,6(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R1,CHKCMP4                                                       
CHKCMPY  CR    RB,RB                                                            
         B     CHKCMPX                                                          
*                                                                               
CHKCMPN  LTR   RB,RB                                                            
CHKCMPX  B     EXIT                                                             
         SPACE                                                                  
CHKCMPE1 MVC   FVMSGNO,=AL2(FVICAM)                                             
         MVC   FVXTRA(L'CMP),CMP          CURRENT CAMPAIGN NUMBER               
         B     CHKCMPX                                                          
         SPACE                                                                  
CHKCMPE2 MVC   FVMSGNO,=AL2(FVNEQDEM)                                           
         B     CHKCMPX                                                          
         EJECT                                                                  
*=========================================*                                     
* READREC - READS THE DETAIL RECORDS      *                                     
* SETS ENDSW TO X'FF'  WHEN FINISHED      *                                     
*=========================================*                                     
*                                                                               
READREC  NTR1                                                                   
         L     R3,AIOAREA1        CURRENT RECORD                                
         USING BWDKEY,R3                                                        
         CLI   FSTINPUT,C'Y'      FIRST INPUT CALL?                             
         BNE   RDREC170           NOPE - GET NEXT RECORD                        
*                                                                               
         MVI   FSTINPUT,C'N'      RESET SWITCH-NEVER AGAIN FIRST TIME           
         MVC   IOKEY(13),DTLKEY   USE SAVED KEY FROM VALQ                       
         LA    R1,MINHI1                                                        
RDREC005 GOTO1 AMIN                                                             
         BE    RDREC010                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     RDNOMORE                                                         
*                                                                               
RDREC010 CLC   IOKEY(BWDKELCD-BWDKEY),DTLKEY                                    
         BNE   RDNOMORE           NO FURTHER MATCH ON DTL                       
*                                                                               
         CLI   FSTA,1             STATION ENTERED?                              
         BNE   RDREC015           NOPE                                          
         CLC   IOKEY(BWDKELPO-BWDKEY),DTLKEY                                    
         BNE   RDRECSEQ           NOT A STATION MATCH                           
*                                                                               
RDREC015 CLI   INOFRM,C'A'        WANT ALL DAYPARTS?                            
         BNE   RDREC040           NO - ONLY WANT DPT REQUESTED                  
         CLI   BDPT,0             ALL DPTS?                                     
         BE    RDREC030           YES                                           
         LA    RE,SUBS            YES - WANT DPT REQUESTED &                    
         LA    RF,L'SUBS          ANY OTHER CORRESPONDING SUB-                  
RDREC020 CLC   BWDDPT,0(RE)       DAYPARTS (SUBS DATA FROM VALDPL)              
         BE    RDREC030                                                         
         CLI   BWDSUBDP,0         IF THERE IS A SUB-DPT                         
         BE    RDREC025                                                         
         CLC   BWDSUBDP,0(RE)     AND IT MATCHES REQUESTED DPT                  
         BE    RDREC030           DISPLAY RECORD INFO                           
RDREC025 LA    RE,1(RE)                                                         
         BCT   RF,RDREC020                                                      
         B     RDRECSEQ           NOT DPT OR MATCHING SUB-DPT (DO SEQ)          
*                                                                               
RDREC030 CLI   BWDSUBDP,0         IF THERE IS A SUB- DAYPART                    
         BE    *+10               SET IT TEMP TO DAYPART FOR ROUTINES           
         MVC   BWDDPT,BWDSUBDP    IN THE 30 PGM                                 
         B     RDREC050                                                         
*                                                                               
RDREC040 CLI   BDPT,0             ALL DPTS?                                     
         BE    RDREC050           YES                                           
         CLC   BWDDPT,BDPT        NOPE -CHECK DAYPART REQUESTED                 
         BNE   RDRECSEQ           NOT A MATCH - DO SEQUENTIAL                   
*                                                                               
RDREC050 CLI   BSLN,0             ALL SPOT LENGTHS?                             
         BE    RDREC060           YES                                           
         CLC   BWDSLN,BSLN        NO - CHECK SPOT LENGTH REQUESTED              
         BNE   RDRECSEQ           NOT A MATCH - DO SEQUENTIAL                   
*                                                                               
RDREC060 BAS   RE,GETRTG          GET RATINGS                                   
* CALL DROOL W/ SAME REC FOR COST1, & (COST2 & COST3 IF ANY)                    
         MVI   COSTIND,1          DEFAULT TO BWDCOST1                           
         OC    BWDEFDT2,BWDEFDT2                                                
         BZ    *+12                                                             
         MVI   COSTIND,2                                                        
         B     RDREC070                                                         
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    *+8                                                              
         MVI   COSTIND,3                                                        
*                                                                               
* IF ALL DAYPART REQUEST MUST BUILD TABLE - OF DAYPARTS ACTUALLY                
* READ FOR LATER USE BY THE GOAL ROUTINE - SAME FOR SPOT LENGTH                 
*                                                                               
RDREC070 DS    0H                                                               
***  AFG  ***                                                                   
         TM    LFLAG,LAFGSFRM      WE DOING AFG+SPECIAL FORMAT?                 
         BO    RDREC130                                                         
***  AFG  ***                                                                   
         CLI   INOFRM,C'A'        ALL DAYPART REQUEST?                          
         BNE   RDREC100                                                         
         CLI   BDPT,0             IF SINGLE DPT - RELEVANT                      
         BNE   RDREC110           DAYPARTS ALREADY ADDED TO TABLE               
         MVC   TMPDPT,BWDDPT                                                    
         BAS   RE,GETSUBS         GET SUB-DAYPARTS FOR THIS DAYPART             
         BNE   RDRECSEQ                                                         
         CLI   SUBTYPE,C'R'       REGULAR DAYPART ?                             
         BNE   RDREC080                                                         
         MVC   TMPDPT,BWDDPT                                                    
         BRAS  RE,ADDPT                                                         
         B     RDREC110                                                         
RDREC080 LA    R1,SUBS                                                          
         LA    R0,L'SUBS                                                        
RDREC090 CLI   0(R1),0                                                          
         BE    RDREC110                                                         
         MVC   TMPDPT,0(R1)                                                     
         BRAS  RE,ADDPT                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,RDREC090                                                      
         B     RDREC110                                                         
*                                                                               
RDREC100 CLI   BDPT,0             ALL DAYPART REQUEST?                          
         BNE   RDREC110           NO - SINGLE DAYPART DON'T BUILD LIST          
         MVC   TMPDPT,BWDDPT                                                    
         BRAS  RE,ADDPT                                                         
*                                                                               
RDREC110 CLI   BSLN,0             ALL SPOT LENGTH REQUEST?                      
         BNE   RDREC130           ONE SPOT LENGTH - DON'T BUILD LIST            
*                                                                               
         CLI   BWDSLN,X'FF'        X'FF' FOR DAYPART TOTAL?                     
         BE    RDREC130            YES, DON'T INCLUDE IN COMSLLST               
*                                                                               
         LA    R1,COMSLLST                                                      
         LA    R0,L'COMSLLST                                                    
RDREC120 CLI   0(R1),0            IF SPOT LENGTH NOT                            
         BNE   *+14               IN LIST                                       
         MVC   0(1,R1),BWDSLN     ADD IT                                        
         B     RDREC130           IF                                            
         CLC   BWDSLN,0(R1)       ALREADY THERE                                 
         BE    RDREC130           DON'T ADD IT AGAIN                            
         LA    R1,1(R1)                                                         
         BCT   R0,RDREC120                                                      
         DC    H'0'                                                             
*                                                                               
RDREC130 BAS   RE,GETSPTS         GET SPOTS INTO TABLE(CNT PNTS AS              
*                                 ( IF NOT WEEKLY)                              
         LA    RE,0                                                             
RDREC140 STC   RE,WEEKNO          START DISPLAYING AT PERIOD START              
         CLC   WEEKNO,PERDISPS                                                  
         BNL   *+12                                                             
         LA    RE,1(RE)                                                         
         B     RDREC140                                                         
*                                                                               
         BAS   RE,REDOSPT         SETS GDTLCST FOR SYSDRIVER                    
         CLI   DAILY,C'Y'         ARE WE DAILY?                                 
         BNE   RDREC150                                                         
         BAS   RE,FDYINWK         GET DISPLACEMENT INTO WEEK                    
         BNE   RDRECSEQ           DON'T WANT IT                                 
         BAS   RE,CHKPER          CHECK WITHIN REQUESTED PERIOD                 
         BNE   RDREC180           TRY NEXT WEEK                                 
*                                                                               
RDREC150 B     RDEXIT             EXITING WILL CALL DROOL FOR INPUT             
**********************************                                              
* AT THIS POINT WE STILL HAVE THE SAME RECORD AS LAST CALL TO DROOL             
*  FOR INPUT                                                                    
**********************************                                              
RDREC170 DS    0H                 SEES IF ANY MORE RECORDS FOR DROOL            
*  FOR WEEKLY FORMAT - CALL DROOL W/ THE SAME REC FOR EACH WK IN CMP            
RDREC180 ZIC   RE,WEEKNO                                                        
         LA    RE,1(RE)                                                         
         STC   RE,WEEKNO                                                        
         CLC   WEEKNO,PERDISPE    MAKE SURE NOT PAST PERIOD END                 
         BH    RDREC190           IT IS                                         
         BAS   RE,REDOSPT                                                       
         CLI   DAILY,C'Y'         ARE WE DAILY?                                 
         BNE   RDREC150                                                         
         BAS   RE,FDYINWK         GET DISPLACEMENT INTO WEEK                    
         BNE   RDRECSEQ           DON'T WANT IT                                 
         BAS   RE,CHKPER          CHECK IF IN RQUESTED PERIOD                   
         BNE   RDREC180                                                         
         B     RDREC150                                                         
*                                                                               
RDREC190 CLI   COSTIND,1          WE DONE WITH ALL THE COSTS?                   
         BNE   RDREC200            NO                                           
         CLC   INOFRM,=C'PT'      BY DAYPART (F=DPT)?                           
         BNE   RDRECSEQ           NO, THEN DO AS WE ALWAYS DID                  
         CLI   BWDSLN,X'FF'       ALREADY ADDED THIS REC TO DPT-255?            
         BNE   *+12                                                             
         MVI   GLMAXTLV,0         TOTAL UP EVERYTHING                           
         B     RDRECSEQ           YES, THEN GET THE NEXT RECORD                 
*                                                                               
*                                 DPT-LEN IS LEVEL 13, SO SETTING THIS          
*                                   SAYS DON'T PUT TOTAL OUT TO SORT IF         
         MVI   GLMAXTLV,14          LEVEL IS LESS THAN THIS                     
         MVI   BWDSLN,X'FF'                                                     
         B     RDREC060           LET DROOL SEE THIS FOR INPUT AS WELL          
*                                                                               
RDREC200 CLI   COSTIND,2                                                        
         BNE   RDREC210                                                         
         MVI   COSTIND,1                                                        
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    RDREC130                                                         
         MVI   COSTIND,3                                                        
         B     RDREC130                                                         
RDREC210 CLI   COSTIND,3                                                        
         BE    *+6                                                              
         DC    H'0'               ONLY POSSIBILITIES ARE 1,2,OR 3               
         MVI   COSTIND,1                                                        
         B     RDREC130                                                         
*                                                                               
RDRECSEQ LA    R1,MINSEQ1         SEQ READ FOR NEXT DTL                         
         B     RDREC005                                                         
*                                                                               
RDNOMORE MVI   ENDSW,X'FF'        MARK NO MORE RECORDS                          
RDEXIT   B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
GETSUBS  NTR1                                                                   
         XC    APPARM,APPARM                                                    
         MVC   APPARM(2),CUAALF                                                 
         MVC   APPARM+2(1),QMED                                                 
         MVC   APPARM+3(1),ESTDMENU                                             
         GOTO1 VDPTRD,APPARM,,WORK,VDMGR                                        
         CLI   APPARM+8,X'FF'                                                   
         BE    GETSBNO                                                          
         LA    RE,WORK                                                          
*                                                                               
GETSB2   CLI   0(RE),0                                                          
         BE    GETSBNO                                                          
         CLC   0(1,RE),TMPDPT                                                   
         BE    *+12                                                             
         LA    RE,5(RE)                                                         
         B     GETSB2                                                           
         ZIC   R1,1(RE)           ISOLATE WHETHER IT BELONGS                    
         SRL   R1,4               TO MASTER/SUB-GROUP                           
         MVI   SUBTYPE,C'R'                                                     
         LTR   R1,R1              ZERO=REGULAR DPT                              
         BZ    GETSBY                                                           
         MVI   SUBTYPE,C'0'       EITHER MASTER OR SUB                          
         LA    R0,L'SUBS                                                        
         LA    R2,SUBS                                                          
         LA    RE,WORK                                                          
         SR    RF,RF                                                            
*                                                                               
GETSB4   CLI   0(RE),0                                                          
         BE    GETSBY                                                           
         IC    RF,1(RE)                                                         
         SRL   RF,4                                                             
         CR    RF,R1              IS THIS SAME GROUP                            
         BNE   GETSB6                                                           
         MVC   0(1,R2),0(RE)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,5(RE)                                                         
         BCT   R0,GETSB4                                                        
         B     GETSBY                                                           
*                                                                               
GETSB6   LA    RE,5(RE)                                                         
         B     GETSB4                                                           
*                                                                               
GETSBNO  LTR   RB,RB                                                            
         XIT1                                                                   
GETSBY   CR    RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
*===========================================================*                   
* GOALS - READ GOAL RECORDS FOR EACH DPT/SLN FOR PTS & DOLS *                   
*         FOR EACH MARKET                                   *                   
*===========================================================*                   
GOALS    NTR1                                                                   
         LA    R8,COMSLLST        R8 ->SPOT LENGTH LIST                         
         A     R8,DISPSLN                                                       
         CLI   FSTGIN,C'Y'        FIRST TIME READING GOALS?                     
         BE    GOALF              YES                                           
*                                                                               
*  NOT FIRST TIME READING GOALS                                                 
*  IF FORMAT IS WEEKLY AND STATION IS NOT IN KEY                                
*  CALL DROOL REPEATLY FOR EACH WEEK IN THE CAMPAIGN                            
*                                                                               
         LA    R4,SVWORK          R4 ->DAYPART LIST                             
         A     R4,DISPDPT                                                       
         ZIC   R2,SLNCNT          RESET SPOT LENGTH COUNTER                     
         CLI   DETL,C'W'          WEEKLY FORMAT?                                
         BNE   GOALA              NO                                            
         CLI   FSTA,1             STATION IN KEY?                               
         BE    GOALA              YES                                           
*                                                                               
         BAS   RE,WKGOALS         CALL DROOL FOR EACH WEEK                      
         BNE   EXIT               EXIT TO DROOL                                 
         TM    INOIND,INOIALG      DID WE DO THIS ONCE FOR ALLGOALS?            
         BNZ   GOAL24              SET TO NOT READ ANYMORE                      
         B     GOAL18             CONTINUE TO NEXT SPOT LENGTH                  
*                                                                               
* - CONTINUE TO NEXT SPOT LENGTH/ DAYPART                                       
*                                                                               
GOALA    CLI   SLNCNT,0           IF END OF SPOT LENGTH TABLE                   
         BE    GOAL16             GET NEXT DAYPART                              
         TM    INOIND,INOIALG      DID WE DO THIS ONCE FOR ALLGOALS?            
         BNZ   GOAL24              SET TO NOT READ ANYMORE                      
         B     GOAL18             OTHERWISE -GET NEXT SPOT LENGTH               
*                                                                               
GOALF    LA    R4,COMDPLST         R4 ->DAYPART LIST                            
         A     R4,DISPSLN                                                       
         MVI   FSTGIN,C'N'         NOT FIRST TIME EVER AGAIN                    
         MVC   SVBYTE,BSLN         SAVE THE SPOT LENGTH                         
         MVC   SVFLAG,BDPT         SAVE DAYPART                                 
         MVC   SVDOPT(1),CMPDPOPT  SAVE CAMPAIGN DAYPART OPTION                 
         XC    SVWORK,SVWORK                                                    
*                                                                               
         CLI   INOFRM,C'A'         ALL DAYPART- ALWAYS LOOK AT TABLE            
         BE    *+12                                                             
         CLI   SVFLAG,0            TEST SINGLE DAYPART REQUEST                  
         BNE   GOAL1               YES                                          
         CLI   1(R4),0             NO-TEST ONLY ONE DAYPART ANYWAY              
         BNE   GOAL2                                                            
         GOTO1 AGETDPT,(R4)        YES-GET IT                                   
         BNE   DPTERR              DAYPART ERROR                                
*                                                                               
GOAL1    MVC   SVWORK(1),BDPT      SINGLE DAYPART                               
         B     GOAL14                                                           
*                                                                               
GOAL2    CLI   CMPDPOPT,C'M'       MULTIPLE DPTS - TEST SUBDPT=MAS              
         BE    *+14                                                             
         MVC   SVWORK(L'COMDPLST),0(R4)     NO                                  
         B     GOAL14                                                           
         LA    R2,L'COMDPLST       YES-BUILD LIST OF ALL DAYPARTS               
         LA    R8,SVWORK                                                        
*                                                                               
GOAL3    CLI   0(R4),0                                                          
         BE    GOAL13                                                           
         MVC   APDUB(1),0(R4)                                                   
         GOTO1 AGETDPT,APDUB                                                    
         BNE   DPTERR                                                           
         CLI   DPTTYPE,C'S'                                                     
         BNE   *+18                                                             
         MVC   DPTSUBS(1),BDPT                                                  
         MVI   DPTSUBS+1,0                                                      
         B     GOAL4                                                            
         MVC   0(1,R8),0(R4)                                                    
         LA    R8,1(R8)                                                         
         CLI   DPTTYPE,C'M'                                                     
         BNE   GOAL12                                                           
*                                                                               
GOAL4    LA    RF,L'DPTSUBS                                                     
         LA    R1,DPTSUBS                                                       
         XC    APFULL,APFULL                                                    
*                                                                               
GOAL5    CLI   0(R1),0                                                          
         BE    GOAL10                                                           
         LA    RE,SVWORK                                                        
*                                                                               
GOAL6    CLI   0(RE),0                                                          
         BNE   *+18                                                             
         MVC   0(1,RE),0(R1)                                                    
         ST    RE,APFULL                                                        
         B     GOAL8                                                            
         CLC   0(1,RE),0(R1)                                                    
         BE    GOAL8                                                            
         LA    RE,1(RE)                                                         
         B     GOAL6                                                            
*                                                                               
GOAL8    LA    R1,1(R1)                                                         
         BCT   RF,GOAL5                                                         
*                                                                               
GOAL10   ICM   R1,15,APFULL                                                     
         BZ    GOAL12                                                           
         LR    R8,R1                                                            
         LA    R8,1(R8)                                                         
*                                                                               
GOAL12   LA    R4,1(R4)                                                         
         BCT   R2,GOAL3                                                         
*                                                                               
GOAL13   MVI   CMPDPOPT,C'S'      FAKE CAMP DPT OPT TO SUBDPT=SEP               
*                                                                               
GOAL14   LA    R4,SVWORK          R4 = A(LAST POSSIBLE DAYPART IN LIST)         
         CLI   0(R4),0            ANY DAYPARTS                                  
         BE    GOAL15                                                           
*                                                                               
         AH    R4,=Y(L'SVWORK-1)  TRY TO STICK IN '$' DAYPART                   
         CLI   0(R4),0            DO WE HAVE AN OPENING FOR '$' DPT?            
         BNE   GOAL15             NO                                            
*                                                                               
         LA    R1,L'SVWORK                                                      
         BCTR  R4,0                                                             
GOAL14A  BCT   R1,*+6                                                           
         DC    H'0'                                                             
         CLI   0(R4),0             HIT LAST DAYPART OF THE LIST?                
         BE    *+12                                                             
         MVI   1(R4),C'$'          YES, PUT '$' DAYPART AFTER LAST ONE          
         B     GOAL15                                                           
         BCT   R4,GOAL14A                                                       
*                                                                               
GOAL15   LA    R4,SVWORK          GET GOALS FOR ALL DPTS INCL '$'               
         XC    DISPDPT,DISPDPT                                                  
         CLI   SVBYTE,0           WAS SPOT LENGTH REQUESTED IN KEY?             
         BE    GOAL16             NO                                            
         LA    R1,COMSLLST                                                      
         A     R1,DISPSLN                                                       
         MVC   0(1,R1),SVBYTE     YES - PUT IT IN LIST                          
         MVI   1(R1),0                                                          
*                                                                               
GOAL16   CLI   0(R4),0            IF WE REACH END OF DAYPART LIST               
         BE    GOAL24             WE ARE DONE                                   
         MVC   BDPT,0(R4)         IF NOT - TRY THIS DAYPART WITH                
         LA    R8,COMSLLST        ALL SPOT LENGTHS                              
         A     R8,DISPSLN                                                       
         LA    R2,L'COMSLLST                                                    
*                                                                               
GOAL18   CLI   0(R8),0            ANY MORE SPOT LENGTHS                         
         BNE   GOAL20             YES                                           
*****                                                                           
         CLC   INOFRM,=C'PT'       F=DPT??                                      
         BNE   GOAL19              NO, DON'T CARE ABOUT DAYPART TOTAL           
         OC    SUBGDOL(L'SUBGDOL+L'SUBGPNT),SUBGDOL                             
         BZ    GOAL19              IF NOTHING, THEN DON'T BOTHER                
         CLI   BSLN,X'FF'          DID WE PROCESS DAYPART TOTAL?                
         BE    GOAL18A             YES, ACTUALLY GET THE NEXT DAYPART           
         LA    R0,COMSLLST         SO WHEN IT COMES BACK WE CONTINUE            
         SR    R8,R0               FROM THE LAST SPOT LENGTH (END)              
         ST    R8,DISPSLN                                                       
         MVI   BSLN,X'FF'                                                       
         L     RE,AGLOBAL              SO DRIVE WON'T INCLUDE THIS              
         MVI   GLMAXTLV-GLOBALD(RE),14   SUBTOTALS AGAIN IN TOTALS              
         L     R1,AIOAREA3                                                      
         MVC   0(4,R1),SUBGDOL                                                  
         MVC   4(4,R1),SUBGPNT                                                  
         B     EXIT                CALL DRIVER FOR INPUT (DPT-255)              
*****                                                                           
GOAL18A  DS    0H                                                               
         L     RE,AGLOBAL                                                       
         MVI   GLMAXTLV-GLOBALD(RE),0   TOTAL UP EVERYTHING                     
*&&DO                                                                           
GOAL19   TM    INOIND,INOIALG      DID WE DO THIS ONCE FOR ALLGOALS?            
         BNZ   GOAL24              SET TO NOT READ ANYMORE                      
*&&                                                                             
GOAL19   LA    R4,1(R4)           NO-- TRY NEXT DAYPART                         
         XC    DISPSLN,DISPSLN    START AT BEGINNING OF SPOT LEN TABLE          
         XC    SUBGDOL,SUBGDOL    NEW SUBTOTALS FOR NEW DAYPART                 
         XC    SUBGPNT,SUBGPNT                                                  
         B     GOAL16                                                           
*                                                                               
GOAL20   MVC   BSLN,0(R8)                                                       
*                                                                               
         CLI   BDPT,C'$'           $ GOALS HAVE A SPOT LENGTH OF 1              
         BNE   GOAL20A                                                          
         MVI   BSLN,1                                                           
         LA    R2,1                                                             
         LA    R8,1(R8)            SO NO MORE LENGTHS                           
         CLI   0(R8),0                                                          
         BNE   *-8                                                              
*                                                                               
GOAL20A  GOTO1 AGETGOAL                                                         
         BE    *+6                                                              
         DC    H'0'               CPP GUIDE NOT FOUND                           
***********************************************************************         
*   IOAREA3+000(004) = TOTAL GOAL DOLLARS                                       
*   IOAREA3+004(004) = TOTAL GOAL POINTS                                        
*   IOAREA3+008(212) = GOAL DOLLARS BY WEEK (53X4)                              
*   IOAREA3+220(212) = GOAL POINTS  BY WEEK (53X4)                              
***********************************************************************         
GOAL21   L     R1,AIOAREA3                                                      
         OC    0(220,R1),0(R1)    ANY GOALS READ?                               
         BNZ   GOAL21A            YES, THIS SHOULD BE GOOD ENUF                 
*                                                                               
         CLI   BDPT,C'$'          $ GOALS?                                      
         BE    *+12               YES, HAVE ONLY ONE LENGTH (LEN=1)             
         LA    R8,1(R8)           NO GOAL -GET NEXT SPOT LENGTH                 
         BCT   R2,GOAL18          IF NO MORE SPOT LENGTHS                       
         LA    R4,1(R4)           BUMP TO NEXT DAYPART                          
         B     GOAL16             AND SEE IF THIS GOAL RECORDS                  
*                                                                               
* IF WEEKLY FORMAT CALL DROOL FOR EACH WEEK,DAYPART,SPOTLEN                     
* IF NOT WEEKLY DROOL WILL BE CALLED ONCE FOR DAYPART,SPOTLEN                   
*                                                                               
GOAL21A  TM    ESTIND,ESTICS2      USING COST2?                                 
         BZ    GOAL21A9            NO, NOTHING TO ADJUST                        
         LA    RE,0(R1)            A(GOAL TO BE ADJUSTED)                       
         ST    RE,APPARM                                                        
         GOTO1 C2ADJ,APPARM                                                     
*                                                                               
         L     R1,AIOAREA3                                                      
         CLI   DETL,C'W'           WEEKLY FORMAT?                               
         BNE   GOAL21A9            NO                                           
         LA    R3,8(R1)            A(1ST WEEKLY GOAL)                           
GOAL21A4 ST    R3,APPARM                                                        
         OC    0(4,R3),0(R3)       ZERO GOAL FOR THE WEEK?                      
         BZ    GOAL21A6            YES, THEN SKIP THIS WEEK                     
         GOTO1 C2ADJ,APPARM                                                     
GOAL21A6 LA    R3,4(R3)            CHECK THE NEXT WEEK                          
         L     R1,AIOAREA3                                                      
         LA    RF,220(R1)          MAKE SURE NOT INTO WEEKLY POINTS             
         CR    R3,RF                                                            
         BL    GOAL21A4                                                         
*                                                                               
GOAL21A9 ICM   RF,15,SUBGDOL                                                    
         ICM   RE,15,0(R1)        GOAL DOLLARS                                  
         AR    RF,RE                                                            
         STCM  RF,15,SUBGDOL                                                    
         ICM   RF,15,TOTGDOL                                                    
         AR    RF,RE                                                            
         STCM  RF,15,TOTGDOL                                                    
*                                                                               
         ICM   RF,15,SUBGPNT                                                    
         ICM   RE,15,4(R1)        GOAL POINTS                                   
         AR    RF,RE                                                            
         STCM  RF,15,SUBGPNT                                                    
         ICM   RF,15,TOTGPNT                                                    
         AR    RF,RE                                                            
         STCM  RF,15,TOTGPNT                                                    
*                                                                               
         CLI   DETL,C'W'         WEEKLY FORMAT?                                 
         BNE   GOAL21D           NO                                             
         CLI   FSTA,1            IS STATION IN KEY?                             
         BE    GOAL21D           YES -DON'T EXIT                                
*                                                                               
GOAL21B  CLC   WEEKNO,PERDISPS                                                  
         BL    GOAL21BB                                                         
         BAS   RE,CHKPER2         CHECK WITHIN REQUEST PERIOD                   
         BE    GOAL21C            YES                                           
*                                 NO - BUMP TO NEXT WEEK                        
GOAL21BB ZIC   RE,WEEKNO                                                        
         LA    RE,1(RE)                                                         
         STC   RE,WEEKNO                                                        
         B     GOAL21B                                                          
*                                                                               
GOAL21C  LA    R0,COMSLLST        SAVE PLACE BEFORE LEAVING                     
         SR    R8,R0                                                            
         ST    R8,DISPSLN          --IN SPOT LENGTH TABLE                       
         LA    R0,SVWORK                                                        
         SR    R4,R0                                                            
         ST    R4,DISPDPT          --IN DAYPART TABLE                           
         STC   R2,SLNCNT           --# OF SPOT LENGTHS LEFT                     
         B     EXIT                                                             
*                                                                               
GOAL21D  LA    R8,1(R8)          NEXT SPOT LENGTH                               
         LA    R0,COMSLLST                                                      
         SR    R8,R0                                                            
         ST    R8,DISPSLN         SAVE DISP. IN SPOT LENGTH TABLE               
         LA    R0,SVWORK                                                        
         SR    R4,R0                                                            
         ST    R4,DISPDPT         SAVE DISP IN DAYPART TABLE                    
*                                                                               
         LTR   R2,R2                                                            
         BNZ   *+12                                                             
         MVI   SLNCNT,0                                                         
         B     GOAL22                                                           
         S     R2,=F'1'                                                         
         STC   R2,SLNCNT                                                        
         B     EXIT                CALL DRIVER FOR INPUT                        
*                                                                               
GOAL22   LA    R4,1(R4)            NEXT DAYPART                                 
         LA    R0,SVWORK                                                        
         SR    R4,R0                                                            
         ST    R4,DISPDPT          SAVE DISP IN DAYPART TABLE                   
         XC    DISPSLN,DISPSLN     RESET TO BEGINNING OF SLN TABLE              
         MVI   SLNCNT,0                                                         
         B     EXIT                CALL DRIVER FOR INPUT                        
*                                                                               
GOAL24   MVC   BSLN,SVBYTE         RESTORE VALUES                               
         MVC   BDPT,SVFLAG                                                      
         MVC   CMPDPOPT,SVDOPT                                                  
         MVI   ENDSW,X'FF'                                                      
*                                                                               
         B     EXIT                                                             
         SPACE                                                                  
DPTERR   MVC   FVMSGNO,=AL2(FVIDPT)                                             
         XC    FVXTRA,FVXTRA                                                    
         MVC   FVXTRA(1),0(R4)    INVALID DAYPART                               
         B     EXIT                                                             
***********************************************************************         
* ADJUST THE GOAL POINTED BY THE ADDRESS STORED IN APPARM BY THE C2             
*                                                                               
* ON ENTRY:    APPARM              A(GOAL)                                      
*                                                                               
* ON EXIT:     APPARM              A(ADJUSTED GOAL)                             
***********************************************************************         
C2ADJ    NTR1                                                                   
         L     R1,APPARM                                                        
         ICM   RE,15,0(R1)         YES, ADJUST GOAL WITH C2 FACTOR              
         CVD   RE,APDUB                                                         
         ZAP   APWORK(16),APDUB                                                 
         SRP   APWORK(16),6,0      MULTIPLY BY  1,000,000                       
         XR    R1,R1                                                            
         ICM   R1,7,ESTPW                                                       
         CVD   R1,APDUB                                                         
         LA    RE,8                NUMBER OF BYTES OF DIVISOR                   
         LA    RF,APDUB            1ST BYTE OF DIVISOR                          
C2ADJ10  CLI   0(RF),0             FIRST BYTE OF DIVISOR                        
         BNE   C2ADJ20                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,C2ADJ10          LAST BYTE SHOULD NEVER BE 0                  
*                                                                               
C2ADJ20  BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         DP    APWORK(16),0(0,RF)                                               
*                                                                               
         AHI   RE,2                HIGH ORDER PORTION IS THE QUOTIENT           
         LA    RF,16                                                            
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         ZAP   APDUB,APWORK(0)                                                  
         CVB   RE,APDUB                                                         
*                                                                               
         L     R1,APPARM           SAVE THE ADJUSTED GOAL                       
         STCM  RE,15,0(R1)                                                      
C2ADJX   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* WEEKLY GOALS                                                                  
*                                                                               
WKGOALS  DS    0H                                                               
         ZIC   R1,WEEKNO          WEEK NUMBER                                   
         LA    R1,1(R1)                                                         
         STC   R1,WEEKNO                                                        
         CLC   WEEKNO,PERDISPE                                                  
         BH    WKGL10                                                           
         B     GOODWK                                                           
*                                                                               
WKGL10   MVI   WEEKNO,0           RESET WEEK NUMBER                             
*                                                                               
         LA    R8,1(R8)           NEXT SPOT LENGTH                              
         ZIC   R2,SLNCNT                                                        
         LTR   R2,R2                                                            
         BNZ   *+12                                                             
         MVI   SLNCNT,0                                                         
         B     BADWK                                                            
         S     R2,=F'1'                                                         
         STC   R2,SLNCNT                                                        
         B     BADWK                                                            
*                                                                               
GOODWK   LTR   RB,RB              SET CONDITION CODE NOT EQUAL                  
         BR    RE                 ALLOWING DROOL TO BE CALLED                   
*                                                                               
BADWK    SR    R0,R0              SET CONDITION CODE TO EQUAL                   
         LTR   R0,R0              ENABLING TO REACH NEXT LENGTH/DPT             
         BR    RE                                                               
         EJECT                                                                  
*=============================================*                                 
* GETRTG - GET'S RATINGS FOR REQUESTED DEMOS  *                                 
*=============================================*                                 
*                                                                               
GETRTG   NTR1                                                                   
         XC    SVFRTG,SVFRTG                                                    
         XC    SV2RTG,SV2RTG                                                    
         L     R3,AIOAREA1                                                      
         USING BWDRECD,R3                                                       
         LA    R1,BWDEL                                                         
GTRTG05  CLI   0(R1),0            END OF RECORD                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'02'        DEMO ELEMENT                                  
         BE    GTRTG15                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GTRTG05                                                          
*                                                                               
GTRTG15  DS    0H                                                               
***  2 DECIMAL                                                                  
         ST    R1,APPARM                                                        
         MVI   APPARM,C'N'         WE HAVE NWS DETAIL RECORD                    
         GOTO1 AADJDEMO,APPARM                                                  
         MVI   APPARM,X'00'        TAKE OFF THE C'N'                            
         L     R1,APPARM           LOAD BACK R1                                 
***  2 DECIMAL                                                                  
         LA    R0,DEMNUM          CLEAR OUT RATINGS                             
         LA    R8,LDEMHLD+2                                                     
GTRTG18  XC    0(4,R8),0(R8)                                                    
         LA    R8,6(R8)                                                         
         BCT   R0,GTRTG18                                                       
*                                 GET NEW RATINGS                               
         LA    R0,DEMNUM          # OF DEMOS                                    
         LA    R8,LDEMHLD         SAVE DEMO AREA                                
         LR    R2,R1              SAVE BEGINNING                                
GTRTG30  LA    RE,L'DMODEMO       LENGTH TO BUMP                                
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,DMODEMO-DMOEL(R1) POINT TO DEMO                               
         CLC   1(2,R1),0(R8)                                                    
         BE    GTRTG40                                                          
         BXLE  R1,RE,*-10                                                       
         B     GTRTG50            THAT DEMO NOT IN RECORD - CHECK NEXT          
*                                                                               
GTRTG40  MVC   2(4,R8),4(R1)                                                    
***  2 DECIMAL                                                                  
         OC    3(3,R8),3(R8)       IS IT ZERO RATINGS?                          
         BNZ   GTRTG40C             - NOPE                                      
         TM    2(R8),X'80'         IS IT AN OVERRIDE?                           
         BO    GTRTG40C             - YUP IT IS                                 
         NI    2(R8),X'FF'-X'40'    - NOPE, TAKE OFF 2 DECIMAL BIT\             
***  2 DECIMAL                                                                  
*                                                                               
GTRTG40C C     R0,=F'6'           FIRST RATING?                                 
         BNE   GTRTG40E                                                         
         MVC   SVFRTG,5(R1)       SAVE IT                                       
GTRTG40E C     R0,=F'5'           2ND RATING?                                   
         BNE   GTRTG50                                                          
         MVC   SV2RTG,5(R1)       SAVE IT TOO                                   
*                                                                               
GTRTG50  LA    R8,6(R8)                                                         
         LR    R1,R2                                                            
         BCT   R0,GTRTG30                                                       
GTRTGX   B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------*             
* GETSPTS - SETS UP SPOTABLE - ARRAY OF SPOTS PER WEEK(14X1)      *             
*           IT ALSO SETS GDTLCST - TOTAL SPOT COUNT FOR CAMPAIGN  *             
*-----------------------------------------------------------------*             
*                                                                               
GETSPTS  NTR1                                                                   
         XC    SPOTABLE,SPOTABLE                                                
         XC    GDTLCST,GDTLCST                                                  
*                                                                               
         LA    R8,BWDEL                                                         
GETSPT5  CLI   0(R8),0            END OF RECORD?                                
         BE    GETSPTX                                                          
         CLI   0(R8),SPWELCDQ     SPOTS PER WEEK ELEMENT?                       
         BE    GETSPT7                                                          
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     GETSPT5            CHECK NEXT ELEMENT                            
*                                                                               
GETSPT7  DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R8)                                                       
         SHI   RE,5                  RE CONTAINS LENGTH OF TABLE                
         LA    R8,SPWPERWK-SPWEL(R8) POINT TO START OF TABLE                    
         EX    RE,GETSPT8                                                       
         B     *+10                                                             
GETSPT8  MVC   SPOTABLE(0),0(R8)                                                
*                                                                               
         XC    APDUB,APDUB        USE FOR DATE                                  
         LR    R2,R5              CAMPAIGN DATES                                
         AHI   R2,CMPDATSD-TWAD                                                 
         ZIC   R8,CMPNWKS         MAX NUM OF DAYS/WEEKS WE'RE HOLDING           
         CLI   COSTIND,1                                                        
         BNE   GETSPT50                                                         
         OC    BWDEFDT2,BWDEFDT2                                                
         BZ    GETSPT87           ONLY ONE EFFECTIVE COST                       
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(0,APDUB)                            
         BAS   RE,GETBEG          FINDS BEGINNING OF WEEK FOR APDUB             
         LA    RE,SPOTABLE                                                      
GETSPT10 CLC   0(6,R2),APDUB                                                    
         BNL   GETSPT20                                                         
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT10                                                      
         B     TOOMNYWK                                                         
*                                                                               
GETSPT20 MVI   0(RE),0            ZERO OUT REST                                 
         LA    RE,1(RE)                                                         
         BCT   R8,GETSPT20                                                      
         B     GETSPT87                                                         
         SPACE 2                                                                
GETSPT50 CLI   COSTIND,2                                                        
         BNE   GETSPT80                                                         
         GOTO1 VDATCON,APPARM,(3,BWDEFDT2),(0,APDUB)                            
         BAS   RE,GETBEG                                                        
         MVC   APWORK+6(6),APDUB  SAVE TEMPORARILY                              
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    GETSPT52           GOOD TILL END OF TABLE                        
         GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(0,APDUB)                            
         BAS   RE,GETBEG                                                        
         MVC   APWORK(6),APDUB                                                  
         MVC   APDUB,APWORK+6     RESTORE                                       
*                                                                               
GETSPT52 LA    RE,SPOTABLE                                                      
GETSPT53 CLC   0(6,R2),APDUB                                                    
         BNL   GETSPT55                                                         
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT53                                                      
TOOMNYWK MVC   BWSMSG(34),=C'MORE THAN 14 WEEKS/DAYS COMBINED!!'                
         OI    BWSMSGH+6,X'80'                                                  
         DC    H'0',C'$ABEND'                                                   
*                                                                               
GETSPT55 OC    BWDEFDT3,BWDEFDT3                                                
         BZ    GETSPT87           GOOD TILL END OF TABLE                        
GETSPT60 CLC   0(6,R2),APWORK                                                   
         BNL   GETSPT65                                                         
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT60                                                      
         B     TOOMNYWK                                                         
*                                                                               
GETSPT65 MVI   0(RE),0            ZERO OUT REST OF TABLE                        
         LA    RE,1(RE)                                                         
         BCT   R8,GETSPT65                                                      
         B     GETSPT87                                                         
         SPACE 2                                                                
GETSPT80 CLI   COSTIND,3                                                        
         BE    GETSPT83                                                         
         DC    H'0'                                                             
*                                                                               
GETSPT83 GOTO1 VDATCON,APPARM,(3,BWDEFDT3),(0,APDUB)                            
         BAS   RE,GETBEG                                                        
         LA    RE,SPOTABLE                                                      
GETSPT85 CLC   0(6,R2),APDUB                                                    
         BNL   GETSPT87                                                         
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT85                                                      
*                                                                               
GETSPT87 LA    R8,SPOTABLE                                                      
         ZIC   R0,CMPNWKS                                                       
         SR    R1,R1                                                            
GETSPT90 ZIC   RE,0(R8)                                                         
         AR    R1,RE                                                            
         LA    R8,1(R8)                                                         
         BCT   R0,GETSPT90                                                      
         STH   R1,GDTLCST         SAVE NUMBER OF SPOTS                          
GETSPTX  B     EXIT                                                             
         EJECT                                                                  
*========================================*                                      
* GETBEG - FINDS THE BEGINNING DAY OF THE*                                      
* THAT APDUB IS IN                       *                                      
*========================================*                                      
*                                                                               
GETBEG   NTR1                                                                   
         GOTO1 VGETDAY,APPARM,APDUB,APFULL                                      
         ZIC   R1,APPARM                                                        
         ZIC   R0,ESTOWSDY                                                      
         CHI   R0,2                                                             
         BNL   GETBEG10                                                         
         BCTR  R1,0                                                             
         LNR   R2,R1                                                            
         B     GETBEG20                                                         
*                                                                               
GETBEG10 CR    R0,R1               TIM, MUST ROLL BACK THE APPROPRIATE          
         BNH   *+8                  NUMBER OF DAYS IF OROT > DAY OF WK          
         AHI   R1,7                                                             
*                                                                               
         SR    R0,R1                                                            
         LNR   R2,R0                                                            
*                                                                               
GETBEG20 GOTO1 VADDAY,APPARM,APDUB,APDUB,(R2)                                   
         XIT1                                                                   
         EJECT                                                                  
*=====================================================*                         
* REDOSPT - RECALCULATES GDTLCST TO BE THE SPOT COUNT *                         
*           FOR THE CURRENT WEEK                      *                         
*=====================================================*                         
*                                                                               
REDOSPT  NTR1                                                                   
         XC    GDTLCST,GDTLCST                                                  
         LA    R8,SPOTABLE                                                      
         ZIC   RE,WEEKNO                                                        
         LTR   RE,RE                                                            
         BZ    REDOS10                                                          
         AR    R8,RE                                                            
REDOS10  MVC   GDTLCST+1(1),0(R8)                                               
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
* FDYINWK - GIVEN BWDDAYS CALCULATES THE RECORD'S DISPLACEMENT *                
*           INTO THE WEEK AND SAVES IT IN DYSADD               *                
* SET CC<>0 NOT INTERESTED IN PACKAGE MASTER OR ORBIT SLAVES   *                
*==============================================================*                
*                                                                               
FDYINWK  NTR1                                                                   
         MVC   APBYTE,BWDDAYS     IN CASE NOT PACKAGE/ORBIT RECORD              
*                                                                               
         TM    BWDINDS,BWDIORB    ORBIT RECORD?                                 
         BZ    FDY10              NO                                            
         CLI   BWDKELSQ,0         IF ORBIT RECORD AND SLAVE                     
         BNE   FDYNO              WE ARE NOT INTERESTED                         
*                                 MASTER -USE START OF WEEK                     
         SR    R8,R8                                                            
         ICM   R8,1,ESTOWSDY      DISP INTO WEEK                                
         BZ    FDY50                                                            
         BCTR  R8,0               I START MONDAY AT ZERO                        
         B     FDY50                                                            
*                                                                               
FDY10    TM    BWDINDS,BWDIPKG    PACKAGE RECORD?                               
         BZ    FDY30              NO- USE BWDDAYS                               
         CLI   BWDKELSQ,0         PACKAGE RECORD AND MASTER                     
         BE    FDYNO              WE ARE NOT INTERESTED                         
         MVC   APBYTE,BWDPODAY    OTHERWISE MUST BE SLAVE - USE IT              
*                                                                               
FDY30    SR    R8,R8              DISPLACEMENT INTO WEEK                        
         CLI   ESTOWSDY,0         OUT OF WEEK ROTATOR?                          
         BE    FDY40                                                            
         CLI   ESTOWSDY,2                                                       
         BE    FDYTUE                                                           
         CLI   ESTOWSDY,3                                                       
         BE    FDYWED                                                           
         CLI   ESTOWSDY,4                                                       
         BE    FDYTHR                                                           
         CLI   ESTOWSDY,5                                                       
         BE    FDYFRI                                                           
         CLI   ESTOWSDY,6                                                       
         BE    FDYSAT                                                           
         CLI   ESTOWSDY,7                                                       
         BE    FDYSUN                                                           
         DC    H'0'                                                             
*                                                                               
FDY40    TM    APBYTE,X'40'       MONDAY?                                       
         BNZ   FDY50                                                            
         LA    R8,1(R8)                                                         
*                                                                               
FDYTUE   TM    APBYTE,X'20'       TUESDAY?                                      
         BNZ   FDY50                                                            
         LA    R8,1(R8)                                                         
*                                                                               
FDYWED   TM    APBYTE,X'10'       WEDNESDAY?                                    
         BNZ   FDY50                                                            
         LA    R8,1(R8)                                                         
*                                                                               
FDYTHR   TM    APBYTE,X'08'       THURSDAY?                                     
         BNZ   FDY50                                                            
         LA    R8,1(R8)                                                         
*                                                                               
FDYFRI   TM    APBYTE,X'04'       FRIDAY?                                       
         BNZ   FDY50                                                            
         LA    R8,1(R8)                                                         
*                                                                               
FDYSAT   TM    APBYTE,X'02'       SATURDAY?                                     
         BNZ   FDY50                                                            
         LA    R8,1(R8)                                                         
*                                                                               
FDYSUN   TM    APBYTE,X'01'       SUNDAY?                                       
         BNZ   FDY50                                                            
         LA    R8,1(R8)                                                         
         CLI   ESTOWSDY,0                                                       
         BNE   FDY40              IF OUT OF WEEK - LOOP BACK                    
         DC    H'0'                                                             
*                                                                               
FDY50    STC   R8,DYSADD                                                        
         CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
FDYNO    LTR   RB,RB              MUST BE PACKAGE MASTER                        
         B     EXIT               OR ORBIT SLAVE - DON'T WANT IT                
         EJECT                                                                  
CHKPER   NTR1                                                                   
         LR    R8,R5                                                            
         AHI   R8,CMPDATSP-TWAD                                                 
         SR    RE,RE                                                            
         ICM   RE,1,WEEKNO                                                      
         BZ    *+8                                                              
         MHI   RE,4                                                             
         AR    R8,RE                                                            
*                                                                               
         GOTO1 VDATCON,APPARM,(2,0(R8)),(0,APWORK)                              
*                                                                               
CHKPER10 ZIC   R8,DYSADD                                                        
         GOTO1 VADDAY,APPARM,APWORK,APWORK+6,(R8)                               
         GOTO1 VDATCON,APPARM,(0,APWORK+6),(2,APHALF)                           
*                                                                               
         CLC   APHALF,INOPER                                                    
         BL    CHKPER20             IF LOWER THAN START, DON'T WANT IT          
         CLC   APHALF,INOPER+2                                                  
         BH    CHKPER20             IF HIGHER THAN START, DON'T WANT IT         
*                                                                               
CHKPER15 SR    R1,R1              IT IS IN REQUESTED PERIOD                     
         LTR   R1,R1                                                            
         B     CHKPERX                                                          
*                                                                               
CHKPER20 LTR   RB,RB              NOT IN REQUESTED PERIOD                       
         B     CHKPERX                                                          
*                                                                               
CHKPERX  B     EXIT                                                             
         SPACE 2                                                                
CHKPER2  NTR1                                                                   
         LR    R8,R5                                                            
         AHI   R8,CMPDATSP-TWAD                                                 
         SR    RE,RE                                                            
         ICM   RE,1,WEEKNO                                                      
         BZ    *+8                                                              
         MHI   RE,4                                                             
         AR    R8,RE                                                            
*                                                                               
         CLC   0(2,R8),INOPER                                                   
         BL    CHKBAD                                                           
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     EXIT                                                             
*                                                                               
CHKBAD   LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*===========================================================*                   
* OUTPUT - CALLED BEFORE DRIVER/DROOL IS CALLED FOR OUTPUT  *                   
*===========================================================*                   
OUTPUT   DS    0H                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         MVI   REPMAXL,20          MAX NUMBER OF LINES PER PAGE NOW 20!         
         OI    REPIND1,REPIONL     ONLINE REPORT                                
         DROP  R3                                                               
         B     EXIT                                                             
         SPACE                                                                  
*=======================*                                                       
* DRIVER HOOK - HEADHK  *                                                       
*=======================*                                                       
*                                                                               
DRHOOK   L     R9,AREP                                                          
         USING REPD,R9                                                          
         OI    REPIND1,REPIONL     ONLINE REPORT                                
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
         MVI   REPMAXL,20          MAX NUMBER OF LINES PER PAGE NOW 20!         
         CLI   GLHOOK,GLHEAD      HEADHOOK                                      
         BNE   EXIT                                                             
         MVC   REPH2+2(4),=C'WEEK'                                              
         CLI   DETL,C'W'                                                        
         BNE   HEADHK10                                                         
         CLI   CMPTAB+2,X'FF'      IS THERE AN X'FF'?                           
         BE    HEADHK15             - YUP, ONLY WANT 1 CAMPAIGN NUMBER          
         MVC   REPH2+1(7),=C'CMP/PRD'                                           
         B     HEADHK15                                                         
*                                                                               
HEADHK10 MVC   REPH2+1(7),=C'STATION'                                           
         CLI   DETL,C'S'                                                        
         BE    HEADHK15                                                         
         MVC   REPH2(8),=C'DPT-LEN '                                            
*                                                                               
HEADHK15 CLI   GVPFORM,C'Y'                                                     
         BE    HEADHK30                                                         
         MVC   REPH1+18(24),=24C'-'                                             
         MVC   REPH1+42(12),=C'DEMOGRAPHICS'                                    
         MVC   REPH1+54(24),=24C'-'                                             
*                                                                               
         LA    R8,LDEMHLD                                                       
         MVC   REPH2+11(7),=C'DOLLARS'                                          
         MVC   REPH2+19(7),COMDNAMS                                             
         MVC   REPH2+30(3),=C'CPM'                                              
         CLI   0(R8),C'I'                                                       
         BE    *+10                                                             
         MVC   REPH2+30(3),=C'CPP'                                              
         LA    R8,6(R8)                                                         
         MVC   REPH2+34(7),COMDNAMS+7                                           
         MVC   REPH2+45(3),=C'CPM'                                              
         CLI   0(R8),C'I'                                                       
         BE    *+10                                                             
         MVC   REPH2+45(3),=C'CPP'                                              
         LA    R8,6(R8)                                                         
         MVC   REPH2+49(7),COMDNAMS+14                                          
         MVC   REPH2+60(3),=C'CPM'                                              
         CLI   0(R8),C'I'                                                       
         BE    *+10                                                             
         MVC   REPH2+60(3),=C'CPP'                                              
         LA    R8,6(R8)                                                         
         MVC   REPH2+65(7),COMDNAMS+21                                          
         MVC   REPH2+75(3),=C'CPM'                                              
         CLI   0(R8),C'I'                                                       
         BE    *+10                                                             
         MVC   REPH2+75(3),=C'CPP'                                              
         B     REPHKX                                                           
*                                                                               
HEADHK30 CLC   =C'A2',INOFRM                                                    
         BNE   HEADHK35                                                         
         XC    REPH1,REPH1                                                      
         XC    REPH2+10(70),REPH2                                               
         MVC   REPH2+10(30),=CL30'PNTS    PCT   DOLLARS    PCT  '               
         MVC   REPH2+40(40),=CL40'PNTS    PCT   DOLLARS     PCT'                
         MVC   REPH1+10(12),=C'-------GOAL('                                    
         MVC   REPH1+22(7),COMDNAMS                                             
         MVC   REPH1+29(9),=C')--------'                                        
         MVC   REPH1+40(15),=C'-----PURCHASED('                                 
         MVC   REPH1+55(7),COMDNAMS                                             
         MVC   REPH1+62(6),=C')-----'                                           
         B     REPHKX                                                           
*                                                                               
HEADHK35 MVC   REPH1+10(8),=C'---GOAL('                                         
         MVC   REPH1+18(7),COMDNAMS                                             
         MVC   REPH1+25(7),=C')------'                                          
         MVC   REPH1+33(12),=C'--PURCHASED('                                    
         MVC   REPH1+45(7),COMDNAMS                                             
         MVC   REPH1+52(3),=C')--'                                              
         MVC   REPH2+10(22),=C'PNTS  DOLLARS      CPP'                          
         MVC   REPH2+33(22),=C'PNTS  DOLLARS      CPP'                          
         CLC   =C'S2',INOFRM                                                    
         BNE   HEADHK40                                                         
         MVC   REPH1+55(2),=C'-('                                               
         MVC   REPH1+57(7),COMDNAMS+7                                           
         MVI   REPH1+64,C')'                                                    
         MVC   REPH2+62(3),=C'CPM'                                              
         CLI   LDEMHLD+6,C'I'                                                   
         BE    *+10                                                             
         MVC   REPH2+62(3),=C'CPP'                                              
         B     HEADHK50                                                         
HEADHK40 MVC   REPH2+56(5),=C'SPOTS'                                            
         MVC   REPH1+62(3),=C'AVG'                                              
         MVC   REPH2+62(3),=C'PTS'                                              
*                                                                               
HEADHK50 MVC   REPH1+66(9),=C'PCT-ACHMT'                                        
         MVC   REPH2+66(9),=C'PNTS-DOLS'                                        
***  IF WE HAVE IMPRESSION AS MAIN DEMO CATEGORY..                              
         MVC   REPH2+29(3),=C'CPM'                                              
         MVC   REPH2+52(3),=C'CPM'                                              
         CLI   LDEMHLD,C'I'        WE HAVE IMPRESSION MAIN DEMO?                
         BE    *+16                 - YUP, DON'T USE CPP!                       
         MVC   REPH2+29(3),=C'CPP'                                              
         MVC   REPH2+52(3),=C'CPP'                                              
***  IF WE HAVE IMPRESSION AS MAIN DEMO CATEGORY..                              
REPHKX   B     EXIT                                                             
         DROP  R9                                                               
         DROP  R4                                                               
         EJECT                                                                  
*==================*                                                            
* LITERAL POOL     *                                                            
*==================*                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DMREAD   DC    CL7'DMREAD '                                                     
*                                                                               
SPARE    DS    5200X                                                            
SPAREX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* ADDPT - THIS SUBROUTINE CONSTRUCTS THE COMDPLST (DAYPART LIST)                
***********************************************************************         
ADDPT    NTR1  BASE=*,LABEL=*                                                   
         LA    R1,COMDPLST        YES - POINT TO DPT LIST                       
         LA    R0,L'COMDPLST                                                    
ADDPT5   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(1,R1),TMPDPT     IF DAYPART NOT IN LIST - ADD IT               
         B     ADDPTX                                                           
         CLC   TMPDPT,0(R1)         IS THIS DAYPART ALREADY IN LIST?            
         BE    ADDPTX             YES - DON'T ADD AGAIN                         
         LA    R1,1(R1)                                                         
         BCT   R0,ADDPT5                                                        
         DC    H'0'                                                             
ADDPTX   J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DPTSETUP - SETUP COMPLETE DAYPART TABLE                                       
***********************************************************************         
*                                                                               
DPTSETUP NTR1  BASE=*,LABEL=*                                                   
         MVC   APWORK(L'IOKEY),IOKEY   SAVE OFF IOKEY (BWHKEY HERE)             
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING DPTHDR,R3           DAYPART MENU RECORD                          
         MVI   DPTKTYPE,X'08'                                                   
         MVC   DPTKAGY,QAGY        2 CHARACTER AGENCY CODE                      
         MVC   DPTKMED,QMED        1 CHARACTER MEDIA CODE                       
         MVC   DPTKMENU,ESTDMENU   DAYPART MENU                                 
*                                                                               
         GOTO1 AIO,DIRHI+IO2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   DPTKEY,IOKEYSAV     DID WE FIND IT?                              
         BE    *+6                                                              
         DC    H'0'                DAYPART MENU NOT THERE, DIE                  
*                                                                               
         GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA2                                                      
         LA    R2,DPTEL                                                         
         XR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R1,R2               R1 SHOULD POINT TO DPTEL2 NOW                
         LA    R2,2(R2)            WE'RE NOW POINTING TO DPTCODES               
*                                                                               
DPTST20  CLI   0(R2),0             ANYTHING HERE                                
         BE    DPTSTX                                                           
         MVC   TMPDPT,0(R2)                                                     
         BRAS  RE,ADDPT                                                         
         LA    R2,5(R2)            BUMP TO THE NEXT DPT LETTER                  
         B     DPTST20                                                          
*                                                                               
***  SETTING UP THE COMPLETE SPOT LENGTH LIST HERE                              
*PTSTX   MVC   COMSLLST,SLNTAB     15 + 1 BYTES AT THE MOMENT                   
DPTSTX   DS    0H                                                               
         L     R1,VSLNTAB          PIONT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RF POINTS TO EOT                             
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMED,C'T'                                                        
         BE    DPTSTXC                                                          
         CLI   QMED,C'N'                                                        
         BE    DPTSTXC                                                          
         CLI   QMED,C'C'                                                        
         BE    DPTSTXC                                                          
         CLI   QMED,C'R'                                                        
         BE    DPTSTXC                                                          
         CLI   QMED,C'X'                                                        
         BE    DPTSTXC                                                          
         DC    H'0'                                                             
*                                                                               
DPTSTXC  CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    DPTSTXG                                                          
         CLC   CUAALF,0(R1)        MATCH AGY ALPHA                              
         BNE   DPTSTXK              - NEXT ENTRY                                
DPTSTXG  CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    DPTSTXN              - YUP, WE'RE GOOD TO GO                     
DPTSTXK  BXLE  R1,RE,DPTSTXC       NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
DPTSTXN  LR    RF,R1               GONNA POINT RF TO END OF TABLE               
         AR    RF,RE                                                            
         AHI   R1,4                POINT BEYOND TABLE ID                        
         MVI   APBYTE,0                                                         
         LA    RE,COMSLLST                                                      
*                                                                               
DPTSTXO  CR    R1,RF               IS IT UP TO THE NEXT TABLE YET?              
         BNL   DPTSTXX              - YUP, WE'RE DONE                           
         CLI   1(R1),0             0 LENGTH?                                    
         BE    DPTSTXQ                                                          
         CLC   APBYTE,1(R1)        DO WE HAVE THIS ALREADY?                     
         BE    DPTSTXQ                                                          
         MVC   APBYTE,1(R1)                                                     
         MVC   0(1,RE),1(R1)       SAVE IT IN THE TABLE                         
*                                                                               
         LA    RE,1(RE)            BUMP IT UP                                   
DPTSTXQ  LA    R1,2(R1)            BUMP IT UP                                   
         B     DPTSTXO                                                          
*                                                                               
DPTSTXX  MVC   IOKEY,APWORK        RESTORE BWHKEY                               
         J     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
**NTAB   DS    0XL1                                                             
**     ++INCLUDE SPSLNTAB                                                       
**       DC    AL1(0)                                                           
**NTABLQ EQU   *-SLNTAB            IT IS CURRENTLY 15 + X'00'                   
         EJECT                                                                  
***********************************************************************         
* SETPER - CALCULATE START & END FOR EACH CAMP                                  
***********************************************************************         
SETPER   NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         OC    INOPER(2),INOPER   ANY START DATE ?                              
         BZ    SETPER15                                                         
*                                                                               
SETPER05 LR    R1,R5              DATES PACKED                                  
         AHI   R1,CMPDATSP-TWAD                                                 
         LA    R8,1               START AT 1ST WK IN CAMPAIGN                   
SETPER10 CLC   0(2,R1),INOPER                                                   
         BNL   SETPER15                                                         
         LA    R2,1(R2)           INCREMENT # OF WKS INTO CAMPAIGN              
         LA    R1,4(R1)           NEXT SET OF PACKED DATES                      
         ZIC   RE,CMPNWKS         # OF WEEKS IN CAMPAIGN                        
         CR    R8,RE                                                            
         BNH   SETPER10                                                         
         SR    R2,R2              IF START DT NOT THERE-USE CAM START           
*                                                                               
SETPER15 STC   R2,PERDISPS                                                      
*                                                                               
SETPER20 ZIC   R1,CMPNWKS                                                       
         BCTR  R1,0                                                             
         STC   R1,PERDISPE             CAMPAIGN END                             
         OC    INOPER+2(2),INOPER+2    ANY PERIOD END?                          
         BZ    SETPER40                                                         
         SR    R2,R2                                                            
         LR    R1,R5                                                            
         AHI   R1,CMPDATSP-TWAD                                                 
         LA    R8,1               START AT 1ST WK IN CAMPAIGN                   
****  DUH, NEED TO CHECK FOR CAMPAIGN'S END, HELLO!!   MHC  09/02/04            
SETPER25 CLI   0(R1),X'FF'        WE HIT END OF CMPDATSP?                       
         BE    SETPER30            - YUP WE DID                                 
         CLC   2(2,R1),INOPER+2                                                 
         BNL   SETPER30                                                         
         LA    R2,1(R2)                                                         
         LA    R1,4(R1)                                                         
         ZIC   RE,CMPNWKS          # OF WEEKS IN CAMPAIGN                       
         CR    R8,RE                                                            
         BNH   SETPER25                                                         
*                                 IF END DT NOT THERE - USE CAM END             
SETPER30 STC   R2,PERDISPE                                                      
*                                                                               
SETPER40 MVI   DAILY,C'N'                                                       
         CLI   INOFRM,C'Y'        ARE WE DAILY?                                 
         BNE   SETPERX                                                          
         MVI   DAILY,C'Y'                                                       
* IF WE ARE DAILY - WE ARE ONLY ALLOWED TWO WEEKS TO DISPLAY                    
         ZIC   R1,PERDISPE        END WEEK                                      
         ZIC   R2,PERDISPS        START WEEK                                    
         SR    R1,R2                                                            
         CHI   R1,1               NO MORE THAN TWO WEEKS                        
         BNH   SETPERX                                                          
* THIS MEANS DIDN'T GET VALIDATED CORRECTLY, ENTERED ONLY A                     
* START DATE, OR DIDN'T ENTER IN A PERIOD WHATSOEVER                            
         MVC   APHALF,INOPER                                                    
         OC    INOPER,INOPER                                                    
         BZ    SETPER50                                                         
*  ENTERED A START BUT NOT A END                                                
         ZIC   R1,PERDISPS                                                      
         AHI   R1,1                                                             
         STC   R1,PERDISPE                                                      
         B     SETPER70           FIX END DISPLACEMENT WEEK                     
*                                                                               
*  DIDN'T ENTER A PERIOD AT ALL - USE START OF CAMPAIGN                         
SETPER50 LR    R2,R5                                                            
         AHI   R2,CMPDATSP-TWAD                                                 
         MVC   APHALF,0(R2)                                                     
         MVI   PERDISPS,0         START OF CAMPAIGN                             
         MVI   PERDISPE,1         END CAMPAIGN                                  
*                                                                               
SETPER70 GOTO1 VDATCON,APPARM,(2,APHALF),(0,APWORK)                             
         GOTO1 VADDAY,APPARM,APWORK,APWORK+6,F'13'                              
         GOTO1 VDATCON,APPARM,(0,APWORK+6),(2,INOPER+2)                         
*                                                                               
SETPERX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* SPNWSWRK                                                                      
       ++INCLUDE SPNWSWRK                                                       
         EJECT                                                                  
* LOCAL STORAGE AREA                                                            
LOCALD   DSECT                                                                  
       ++INCLUDE SPNWSDRVD                                                      
*                                                                               
         DS    0F                                                               
DISPSLN  DS    F                                                                
DISPDPT  DS    F                                                                
*                                                                               
SAVERE   DS    A                   TO SAVE OFF RE                               
SAVER9   DS    A                   TO SAVE OFF R9 (FOR INPUT)                   
*                                                                               
LFLAG    DS    XL1                FLAG FOR KEY1/KEY2                            
LVALKEY2 EQU   X'01'                                                            
LAFGSFRM EQU   X'80'               AFG WITH SPECIAL FORMAT FLAG                 
*                                   - ALL, AL2, DPT                             
*                                                                               
FSTINPUT DS    CL1                 FIRST TIME THRU DETAIL INPUT                 
FSTGIN   DS    CL1                                                              
FSTA     DS    CL1                                                              
SLNCNT   DS    CL1                                                              
ENDSW    DS    CL1                 END OF READS SWITCH                          
SVBYTE   DS    CL1                                                              
SVFLAG   DS    CL1                                                              
DETL     DS    CL1                W=WEEK S=STATION                              
GVPFORM  DS    CL1                Y=GVP, N=CPP/CPM FORMAT                       
PERDISPS DS    XL1                START DISP INTO CAMPAIGN PERIOD               
PERDISPE DS    XL1                END DISP INTO CAMPAIGN PERIOD                 
CURCMP   DS    XL2                CURRENT BINARY CAMPAIGN NUMBER                
FLAG     DS    CL1                                                              
SVCMP2   DS    XL2                END BINARY CAMP NUMBER                        
SVCMP1   DS    XL2                START BINARY CAMP NUMBER                      
CMP      DS    CL(L'QCAM)         TEMP CAMPAIGN                                 
*                                                                               
FAKEFLDH DS    CL8                FOR SCANNER CALL IN CMPSETUP                  
FAKEFLD  DS    CL16                           MHC  06/16/03                     
CMPTAB   DS    XL37               CAMPAIGN TABLE                                
*  LAST CMPTAB BYTE FOR X'FF' --- END OF CMPTAB (18 ENTRIES ONLY!!)             
MYSAVBV  DS    CL(BVALSX-BMKT)    SAVE VALUES                                   
MYSAVQV  DS    CL(QVALSX-QMKT)    SAVE VALUES                                   
*                                                                               
SVDOPT   DS    CL2                                                              
*COMSLLST DS    CL8 ---> CL15+1    SPOT LENGTHS FOR GETTING GOALS               
*COMSLLST DS    CL(SLNTABLQ)       EXPANDED 4 ALL POSSIBLE LENGTHS (16)         
COMSLLST DS    CL255              SLNTAB NOW CORERES WITH ALL SPOT LENS         
COMDPLST DS    CL36               DAYPART LIST FOR GETTING GOALS                
SVWORK   DS    CL(L'COMDPLST)     SAVED DAYPART LIST                            
*                                                                               
TMPDPT   DS    CL1                TEMP. DAYPART                                 
SUBS     DS    CL17               SAVED SUB-DAYPART LIST                        
SUBTYPE  DS    CL1                DAYPART TYPE FROM GETSUB                      
*                                                                               
       ++INCLUDE DEDBLOCK                                                       
         ORG   LOCALD+LDRVRLOC                                                  
*                                                                               
         ORG   LOCALD+4096                                                      
*                                                                               
LOCALX   EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSF9D                                                       
         EJECT                                                                  
* SPNWSHDR                                                                      
* SPNWSCAM                                                                      
* SPGENDAYPT                                                                    
* DRGLOBAL AND DROOLLOCAL                                                       
* FAFACTS                                                                       
* FATIOB                                                                        
* DDSCANBLKD                                                                    
       ++INCLUDE SPNWSHDR                                                       
       ++INCLUDE SPNWSCAM                                                       
       ++INCLUDE SPGENDAYPT                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DROOLLOCAL                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDSCANBLKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058SPNWS10   02/26/07'                                      
         END                                                                    
