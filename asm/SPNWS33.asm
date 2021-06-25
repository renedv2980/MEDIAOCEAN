*          DATA SET SPNWS33    AT LEVEL 119 AS OF 02/27/07                      
*PHASE T20733C,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'NWS33 - BUYERS WORK SHEET - BUYS RECAP'                         
T20733   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20733**,RA,RR=RE                                              
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
         BNE   VALQ01                                                           
         MVC   INOFRM,=C'S2'                                                    
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
VALCAM   CLI   FVINDX,3           THRID FIELD?                                  
         BNE   VALMOS             NO - TRY NEXT FIELD                           
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
         ICM   R1,3,CMPTAB       JUST NEED THE FIRST NUMBER IN TABLE            
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
VALCAM3D CLM   RE,1,CMPNWKS        MAKE SURE WE DON'T PAST # OF WEEKS           
         BNL   VALCAM3X              FOR CAMPAIGN                               
*                                                                               
         TM    MISCFLG1,MF1FSTTM   FIRST TIME?                                  
         BNZ   VALCAM3X            YES, DON'T USE RCPNWKS                       
*                                                                               
         CLM   RE,1,RCPNWKS                                                     
         BL    VALCAM3E                                                         
         IC    RE,RCPNWKS                                                       
         BCTR  RE,0                                                             
VALCAM3E STC   RE,RCPDSTDT                                                      
*                                                                               
VALCAM3X BAS   RE,GETCPE          GET CLIENT/PROD/EST DETAILS                   
         BNE   VALQX                                                            
***  AFG  ***                                                                   
         TM    INOIND,INOIALG      ARE WE DOING ALL GOALS?                      
         BZ    VALCAM3G             - NOPE, NORMAL                              
*                                                                               
         OC    INOFRM,INOFRM       TEST FORMAT OPTION SET                       
         BZ    VALCAM3G                                                         
         CLC   =C'PT',INOFRM       DAYPART?                                     
         BE    VALCAM3C             - YUP!                                      
         CLC   =C'AL',INOFRM       ALL?                                         
         BE    VALCAM3C             - YUP!                                      
         CLC   =C'A2',INOFRM       ALL 2?                                       
         BNE   VALCAM3G            NONE OF THE ABOVE, CONTINUE                  
*                                                                               
VALCAM3C BRAS  RE,DPTSETUP         LETS SETUP OUR COMPLETE DAYPART LIST         
*                                  ...SETS UP SPOT LENGTH LIST AS WELL          
         NI    INOIND,X'FF'-INOIALG   TRN OFF FLAG SO 00 TREAT NORMALLY         
         OI    LFLAG,LAFGSFRM      TURN ON LOCAL AFG+SPECIAL FORMAT FLG         
***  AFG  ***                                                                   
VALCAM3G XC    LDEMHLD,LDEMHLD    MOVE EST INFO TO LDEMHLD                      
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
         CLI   CMPDPOPT,C'M'      TEST SUBDPTS SCHEDULED UNDER MASTER           
         BNE   VALDPL2                                                          
         CLI   DPTTYPE,C'S'       YES -TEST SUBDAYPART                          
         BE    VALQ96             YES - ERROR                                   
*                                                                               
VALDPL2  CLI   INOFRM,C'A'        ALL DAYPARTS?                                 
         BNE   VALQ32             NO - GET NEXT KEY VALUE                       
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
         BAS   RE,BLDBKEY         BUILD BUY KEY FOR READREC                     
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
         B     EXITYES                                                          
GETCPEN  B     EXITNO                                                           
         EJECT                                                                  
***********************************************************************         
* BLDBKEY  - BUILDS BUY KEY                                                     
***********************************************************************         
BLDBKEY  NTR1                                                                   
         MVC   IOKEY,HDRKEY       READ HEADER POINTER                           
         GOTO1 AIO,DIRHI+IO1                                                    
         BNE   BLDBN                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BE    BLDB10                                                           
         OI    TWAFLAG,TWANOHDR   NO                                            
         LA    R1,BWSKEYH         SET CURSOR TO KEY FIELD                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     BLDBN                                                            
*                                                                               
BLDB10   MVC   HDRDA,IODA         SAVE HEADER D/A                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO1   GET HEADER RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         USING BWHKEY,R2                                                        
         MVC   BCMSEQ,BWHKSEQ     SAVE SEQUENCE NUMBER                          
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING BUYKEY,R2                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,BPRD                                                     
         MVC   BUYMSTA(L'BMKT+L'BSTA),BMKT                                      
         MVC   BUYKEST,CMPESTN                                                  
         DROP  R2                                                               
         MVC   DTLKEY,IOKEY                                                     
*&&DO                                                                           
BLDB45   MVC   HDRDA,IODA         SAVE HEADER D/A                               
         GOTO1 AIO,IOSPTFIL+IOGET+IO1   GET HEADER RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         USING BWHKEY,R2                                                        
         MVC   BCMSEQ,BWHKSEQ     SAVE SEQUENCE NUMBER                          
         SR    RE,RE                                                            
         MVI   BSTACD,0                                                         
         CLI   FSTA,1             TEST STATION REQUESTED?                       
         BNE   BLDB50                                                           
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
         SR    RE,RE                                                            
*                                                                               
BLDB46   CLI   0(R4),0                                                          
         BE    BLDB48                                                           
         CLI   0(R4),BWHELCDQ                                                   
         BNE   BLDB47                                                           
         USING BWHEL,R4                                                         
         IC    RE,BWHSEQ                                                        
         CLC   QSTA,BWHSTA                                                      
         BNE   BLDB47                                                           
         STC   RE,BSTACD          SET STATION SEQ NO                            
         B     BLDB50                                                           
*                                                                               
BLDB47   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BLDB46                                                           
*                                                                               
BLDB48   OI    TWAFLAG,TWANOSTA   NO STATION FOUND                              
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         LA    R1,BWSKEYH                                                       
         ST    R1,FVADDR                                                        
         B     BLDBN                                                            
*                                                                               
BLDB50   LA    R3,DTLKEY          BUILD DETAIL KEY                              
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
*&&                                                                             
         B     EXITYES                                                          
BLDBN    B     EXITNO                                                           
****     DROP  R2,R3                                                            
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
         LHI   R1,SAVAREA-BWSKEYH                                               
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
         MVI   APPARM,RECBUY       RECORD=WORK                                  
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
INIT     MVI   MISCFLG1,MF1FSTTM  FIRST TIME FOR INPUT SWITCH                   
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
         GOTO1 VCOLY,APPARM,(X'32',0),0,0                                       
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
         LHI   RF,SPAREX-SPARE                                                  
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
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
         L     R9,SAVER9                                                        
*                                                                               
         ICM   RF,3,FATMAXIO      MAXIMUM ALLOWABLE IOS                         
         MHI   RF,95                                                            
         D     RE,=F'100'         95 PERCENT OF MAX IOS IN R3                   
         CLM   RF,3,FATIOCNT      TEST RUNNING OUT OF IOS                       
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
INPUT20  GOTO1 =A(GOALS),RR=APRELO                                              
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
INPUT40  BAS   RE,BLDBKEY         SET KEY FOR READREC                           
         BE    INPUT45                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     INPUT30                                                          
INPUT45  MVI   MISCFLG1,MF1FSTTM   RESET SOME INDICATORS                        
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
INPUTX   B     EXIT                                                             
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
CHKCMPY  B     EXITYES                                                          
*                                                                               
CHKCMPN  B     EXITNO                                                           
         SPACE                                                                  
CHKCMPE1 MVC   FVMSGNO,=AL2(FVICAM)                                             
         MVC   FVXTRA(L'CMP),CMP          CURRENT CAMPAIGN NUMBER               
         B     EXIT                                                             
         SPACE                                                                  
CHKCMPE2 MVC   FVMSGNO,=AL2(FVNEQDEM)                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READS THE BUY RECORDS AND BUY REVISION RECORDS                                
*                                                                               
* SETS ENDSW TO X'FF'  WHEN FINISHED                                            
***********************************************************************         
READREC  NTR1                                                                   
         TM    MISCFLG1,MF1MANUL   UPTO MANUALLY ADDED REVISIONS?               
         BNZ   RREC100             YES                                          
         LA    R2,IOKEY           CURRENT RECORD                                
         USING BUYKEY,R2                                                        
         TM    MISCFLG1,MF1FSTTM                                                
         BZ    RREC47             NOPE - GET NEXT RECORD                        
*                                                                               
         NI    MISCFLG1,X'FF'-MF1FSTTM  SWITCH-NEVER AGAIN FIRST TIME           
         MVC   IOKEY(13),DTLKEY   USE SAVED KEY FROM VALQ                       
RRECRDHI LA    R1,DIRHI+IO1                                                     
         B     RRECIO                                                           
RRECRHSQ LA    R1,DIRHI+IO1       HIGH AND THEN A SEQUENTIAL                    
         GOTO1 AIO                   FOR THOSE CASES WHERE DPT REC READ         
RRECRDSQ LA    R1,DIRSQ+IO1                                                     
*                                                                               
RRECIO   GOTO1 AIO                                                              
         BNE   RREC99                                                           
*                                                                               
         LA    R2,IOKEY           CURRENT RECORD                                
         LA    RE,BUYMSTA+2-BUYKEY   MAKE SURE THAT THE BUY IS THE SAME         
         CLI   FSTA,1                  UPTO THE MARKET UNLESS WE HAVE A         
         BNE   *+8                     STATION FILTER WHERE WE'LL HAVE          
         LA    RE,BUYKBUY-BUYKEY       TO CHECK UPTO THE BUYLINE NUMBER         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   RREC99             NO FURTHER MATCH ON DTL                       
*                                                                               
         CLI   FSTA,1             DO WE HAVE A STATION FILTER?                  
         BE    RREC8              YES, EST SHOULD HAVE BEEN CHECKED             
         OC    QCABLE,QCABLE      IF CABLE, QCABLE IS SET, NOT QSTA             
         BZ    RREC2                 ....  WHY TIM, WHY?                        
*                                                                               
         GOTO1 VMSUNPK,APPARM,(X'80',BUYMSTA),APWORK,APWORK+L'QMKT              
         CLC   QCABLE(4),APWORK+L'QMKT                                          
         BE    RREC8               MATCH ON CABLE STATION                       
         BL    RREC99              NO, DO MANUALLY ADDED REVISIONS THEN         
         B     RREC4                                                            
*                                                                               
RREC2    CLC   BUYKEST,CMPESTN     DOES BUY EST MATCH CAMP EST?                 
         BE    RREC8               YES                                          
         BL    RREC6               LESS THAN, SET TO CAMP EST AND TRY           
         SR    RE,RE               GREATER, BUMP STA AND SET EST                
RREC4    ICM   RE,7,BUYMSTA+2                                                   
         LA    RE,1(RE)                                                         
         STCM  RE,7,BUYMSTA+2                                                   
*                                                                               
RREC6    MVC   BUYKEST,CMPESTN                                                  
         XC    BUYKBUY,BUYKBUY                                                  
         B     RRECRDHI                                                         
*                                                                               
RREC8    GOTO1 AIO,FILGET1                                                      
         BE    *+14                                                             
         CLI   IOERR,X'02'         RECORD IS DELETED THOUGH                     
         BE    RRECRDSQ                                                         
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
***************                                                                 
         CLC   BMKT,BUYMSTA        SPILL BUY?                                   
         BNE   RRECRDSQ            YES                                          
***************                                                                 
         CLC   BDSTART,CMPST       HAVE AN INTERSECTION HERE?                   
         BH    RREC8A                                                           
         CLC   BDEND,CMPST                                                      
         BL    RRECRDSQ            NO                                           
         B     RREC9                                                            
*                                                                               
RREC8A   CLC   BDSTART,CMPND                                                    
         BH    RRECRDSQ            NO                                           
*                                                                               
RREC9    NI    MISCFLG1,X'FF'-MF1MANUL-MF1COSOV                                 
         MVC   SVBUYKEY,IOKEY    SAVE BUY KEY WE'RE UPTO                        
         XC    IOKEY,IOKEY                                                      
         LA    R1,IOKEY                                                         
         USING NBRKEY,R1           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         MVC   NBRKSTA,BUYMSTA+L'BMKT                                           
         MVC   NBRKKBUY,SVBUYKEY+BUYKBUY-BUYKEY                                 
         DROP  R1                                                               
*                                                                               
         L     RE,AIOAREA2                                                      
****     LA    RF,4000                                                          
         LHI   RF,6000                                                          
         XCEFL                                                                  
*                                                                               
         GOTO1 AIO,DIRHI+IO2                                                    
         BNE   RREC9A                                                           
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV      GOT A BUY REVISION REC?            
         BNE   RREC9A                                                           
*                                                                               
         GOTO1 AIO,FILGET2                                                      
         BE    RREC9A                                                           
         DC    H'0'                                                             
*                                                                               
RREC9A   XC    IOKEY,IOKEY         RESTORE THE BUY KEY SO WE CAN DO A           
         MVC   IOKEY(L'BUYKEY),SVBUYKEY SEQUENTIAL CORRECTLY                    
         GOTO1 AIO,DIRHI+IO1                                                    
*                                                                               
         L     R3,AIOAREA2         DID WE READ IN A BUY REVISION REC?           
         CLI   0(R3),0                                                          
         BNE   RREC9B              YES                                          
*                                                                               
         GOTO1 ASTUPNBR,APPARM,AIOAREA1,AIOAREA2,DBLOCK                         
******************                                                              
**  HAVE TO RE-ESTABLISH READ SEQ BECAUSE OF GETDPT IN ASTUPNBR                 
******************                                                              
         XC    IOKEY,IOKEY         RESTORE THE BUY KEY SO WE CAN DO A           
         MVC   IOKEY(L'BUYKEY),SVBUYKEY SEQUENTIAL CORRECTLY                    
         GOTO1 AIO,DIRHI+IO1                                                    
******************                                                              
* THE FOLLOWING LINES ARE DIFFERENT FOR SPNWS33 THAN EVERY OTHER                
* MODULES THAT CALLS SETUPNBR                                                   
         L     RE,AIOAREA2                                                      
         USING NBRKEY,RE                                                        
         MVC   NBRKKBUY,BUYKBUY-BUYKEY+IOKEY                                    
         DROP  RE                                                               
******************                                                              
*                                                                               
         USING NBRKEY,R3                                                        
RREC9B   LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
RREC10   DS    0H                                                               
***  AFG  ***                                                                   
         TM    LFLAG,LAFGSFRM      WE DOING AFG+SPECIAL FORMAT?                 
         BO    RREC30               - YUP WE ARE                                
***  AFG  ***                                                                   
         CLI   INOFRM,C'A'        WANT ALL DAYPARTS?                            
         BNE   RREC20             NO - ONLY WANT DPT REQUESTED                  
         CLI   BDPT,0             ALL DPTS?                                     
         BE    RREC19             YES                                           
         LA    RE,SUBS            YES - WANT DPT REQUESTED &                    
         LA    RF,L'SUBS          ANY OTHER CORRESPONDING SUB-                  
RREC15   CLC   NBRSDYPT,0(RE)     DAYPARTS (SUBS DATA FROM VALDPL)              
         BE    RREC19                                                           
         CLI   NBRSSBDP,0         IF THERE IS A SUB-DPT                         
         BE    RREC18                                                           
         CLC   NBRSSBDP,0(RE)     AND IT MATCHES REQUESTED DPT                  
         BE    RREC19             DISPLAY RECORD INFO                           
RREC18   LA    RE,1(RE)                                                         
         BCT   RF,RREC15                                                        
         B     RRECRDSQ           NOT DPT OR MATCHING SUB-DPT (DO SEQ)          
RREC19   DS    0H                                                               
         CLI   NBRSSBDP,0         IF THERE IS A SUB- DAYPART                    
         BE    *+10               SET IT TEMP TO DAYPART FOR ROUTINES           
         MVC   NBRSDYPT,NBRSSBDP    IN THE 30 PGM                               
         B     RREC25                                                           
*                                                                               
RREC20   CLI   BDPT,0             ALL DPTS?                                     
         BE    RREC25             YES                                           
         CLC   NBRSDYPT,BDPT      NOPE -CHECK DAYPART REQUESTED                 
         BNE   RRECRDSQ           NOT A MATCH - DO SEQUENTIAL                   
*                                                                               
RREC25   CLI   BSLN,0              ALL SPOT LENGTHS?                            
         BE    RREC30              YES                                          
         CLC   NBRSSLN,BSLN                                                     
         BNE   RRECRDSQ                                                         
*                                                                               
RREC30   BAS   RE,GETRTG          GET RATINGS                                   
* CALL DROOL W/ SAME REC FOR COST1 (, & COST2 & COST3 IF ANY)                   
         MVI   COSTIND,1          DEFAULT TO BWDCOST1                           
*                                                                               
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    *+12                                                             
         MVI   COSTIND,2                                                        
         B     RREC32                                                           
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    *+8                                                              
         MVI   COSTIND,3                                                        
*                                                                               
* IF ALL DAYPART REQUEST MUST BUILD TABLE - OF DAYPARTS ACTUALLY                
* READ FOR LATER USE BY THE GOAL ROUTINE - SAME FOR SPOT LENGTH                 
*                                                                               
RREC32   DS    0H                                                               
***  AFG  ***                                                                   
         TM    LFLAG,LAFGSFRM      WE DOING AFG+SPECIAL FORMAT?                 
         BO    RREC43                                                           
***  AFG  ***                                                                   
         CLI   INOFRM,C'A'        ALL DAYPART REQUEST?                          
         BNE   RREC38                                                           
         CLI   BDPT,0             IF SINGLE DPT - RELEVANT                      
         BNE   RREC39             DAYPARTS ALREADY ADDED TO TABLE               
         MVC   TMPDPT,NBRSDYPT                                                  
         BAS   RE,GETSUBS         GET SUB-DAYPARTS FOR THIS DAYPART             
         BNE   RRECRHSQ            DPT RECORD READ                              
         CLI   SUBTYPE,C'R'       REGULAR DAYPART ?                             
         BNE   RREC36                                                           
         MVC   TMPDPT,NBRSDYPT                                                  
         BRAS  RE,ADDPT                                                         
         B     RREC39                                                           
RREC36   LA    R1,SUBS                                                          
         LA    R0,L'SUBS                                                        
RREC37   CLI   0(R1),0                                                          
         BE    RREC39                                                           
         MVC   TMPDPT,0(R1)                                                     
         BRAS  RE,ADDPT                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,RREC37                                                        
         B     RREC39                                                           
*                                                                               
RREC38   CLI   BDPT,0             ALL DAYPART REQUEST?                          
         BNE   RREC39             NO - SINGLE DAYPART DON'T BUILD LIST          
         MVC   TMPDPT,NBRSDYPT                                                  
         BRAS  RE,ADDPT                                                         
*                                                                               
RREC39   CLI   BSLN,0             ALL SPOT LENGTH REQUEST?                      
         BNE   RREC43             ONE SPOT LENGTH - DON'T BUILD LIST            
*                                                                               
         CLI   NBRSSLN,X'FF'       X'FF' FOR DAYPART TOTAL?                     
         BE    RREC43              YES, DON'T INCLUDE IN COMSLLST               
*                                                                               
         LA    R1,COMSLLST                                                      
         LA    R0,L'COMSLLST                                                    
RREC42   CLI   0(R1),0            IF SPOT LENGTH NOT                            
         BNE   *+14               IN LIST                                       
         MVC   0(1,R1),NBRSSLN    ADD IT                                        
         B     RREC43             IF                                            
         CLC   NBRSSLN,0(R1)      ALREADY THERE                                 
         BE    RREC43             DON'T ADD IT AGAIN                            
         LA    R1,1(R1)                                                         
         BCT   R0,RREC42                                                        
         DC    H'0'                                                             
*                                                                               
RREC43   BAS   RE,GETSPTS         GET SPOTS INTO TABLE(CNT PNTS AS              
*                                 ( IF NOT WEEKLY)                              
         LA    RE,0                                                             
RREC44   STC   RE,WEEKNO          START DISPLAYING AT PERIOD START              
         CLC   WEEKNO,PERDISPS                                                  
         BNL   *+12                                                             
         LA    RE,1(RE)                                                         
         B     RREC44                                                           
*                                                                               
         BAS   RE,REDOSPT         SETS GDTLCST FOR SYSDRIVER                    
         CLI   DAILY,C'Y'         ARE WE DAILY?                                 
         BNE   RREC45                                                           
         BAS   RE,FDYINWK         GET DISPLACEMENT INTO WEEK                    
         BNE   RRECRHSQ           DON'T WANT IT                                 
         BAS   RE,CHKPER          CHECK WITHIN REQUESTED PERIOD                 
         BNE   RREC55             TRY NEXT WEEK                                 
*                                                                               
RREC45   B     RDEXIT             EXITING WILL CALL DROOL FOR INPUT             
*                                                                               
*                                                                               
*                                                                               
RREC47   DS    0H                 SEES IF ANY MORE RECORDS FOR DROOL            
         L     R3,AIOAREA2                                                      
         USING NBRKEY,R3                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*  FOR WEEKLY FORMAT - CALL DROOL W/ THE SAME REC FOR EACH WK IN CMP            
RREC55   XR    RE,RE                                                            
         IC    RE,WEEKNO                                                        
         LA    RE,1(RE)                                                         
         STC   RE,WEEKNO                                                        
         CLC   WEEKNO,PERDISPE    MAKE SURE NOT PAST PERIOD END                 
         BH    RREC60             IT IS                                         
         BAS   RE,REDOSPT                                                       
         CLI   DAILY,C'Y'         ARE WE DAILY?                                 
         BNE   RREC45                                                           
         BAS   RE,FDYINWK         GET DISPLACEMENT INTO WEEK                    
         BNE   RRECRHSQ           DON'T WANT IT                                 
         BAS   RE,CHKPER          CHECK IF IN RQUESTED PERIOD                   
         BNE   RREC55                                                           
         B     RREC45                                                           
*                                                                               
RREC60   CLI   COSTIND,1          SEE IF THERES ANY MORE RECORDS                
         BNE   RREC70                                                           
         OC    NBRCSTBL,NBRCSTBL                                                
         BZ    RREC65                                                           
         OI    MISCFLG1,MF1COSOV                                                
         CLC   NBRCSPTR,NBRCSNUM                                                
         BL    RREC43                                                           
*                                                                               
RREC65   CLC   INOFRM,=C'PT'       F=DPT?                                       
         BNE   RRECRHSQ                                                         
         CLI   NBRSSLN,X'FF'       ALREADY ADDED THIS REC TO DPT-255?           
         BNE   *+12                                                             
         MVI   GLMAXTLV,0          TOTAL UP EVERYTHING                          
         B     RRECRHSQ                                                         
*                                                                               
*                                 OUR LEVEL IS NOW 13, SO SETTING THIS          
*                                   SAYS DON'T PUT TOTAL OUT TO SORT IF         
         MVI   GLMAXTLV,14          LEVEL IS LESS THAN THIS                     
         MVI   NBRSSLN,X'FF'                                                    
         NI    MISCFLG1,X'FF'-MF1COSOV  START ALL OVER WITH LEN OF 255          
         B     RREC30             LET DROOL SEE THIS FOR INPUT AS WELL          
*                                                                               
*                                                                               
RREC70   CLI   COSTIND,2                                                        
         BNE   RREC80                                                           
         MVI   COSTIND,1                                                        
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    RREC43                                                           
         MVI   COSTIND,3                                                        
         B     RREC43                                                           
RREC80   CLI   COSTIND,3                                                        
         BE    *+6                                                              
         DC    H'0'               ONLY POSSIBILITIES ARE 1,2,OR 3               
         MVI   COSTIND,1                                                        
         B     RREC43                                                           
*                                                                               
RREC99   XC    IOKEY,IOKEY                                                      
         NI    MISCFLG1,X'FF'-MF1COSOV                                          
         OI    MISCFLG1,MF1FSTTM+MF1MANUL  1ST TIME FOR MANUALLY ADDED          
         MVI   NBRCSNUM,0                                                       
         MVI   NBRCSPTR,0                                                       
         XC    NBRCSTBL,NBRCSTBL                                                
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2           LOOK FOR NWS BUY REVISION RECORD             
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ      BINARY CAMPAIGN/MARKET SEQ #                 
         OC    QSTA,QSTA                                                        
         BZ    *+10                                                             
         MVC   NBRKSTA,BSTA                                                     
         DROP  R2                                                               
         EJECT                                                                  
**********************************                                              
* READ MANUALLY ADDED BUY REVISIONS                                             
**********************************                                              
RREC100  DS    0H                                                               
         L     R2,AIOAREA2                                                      
         USING NBRKEY,R2           LOOK FOR NWS BUY REVISION RECORD             
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
         TM    MISCFLG1,MF1FSTTM                                                
         BZ    RREC147                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1FSTTM                                          
RREC1RHI LA    R1,DIRHI+IO2                                                     
         B     RREC1IO                                                          
RREC1RSQ LA    R1,DIRSQ+IO2                                                     
*                                                                               
RREC1IO  GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     RREC200                                                          
*                                                                               
         LA    RE,NBRKSTA-NBRKEY-1                                              
         CLI   FSTA,1                                                           
         BNE   *+8                                                              
         LA    RE,NBRKKBUY-NBRKEY-1                                             
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV      GOT A BUY REVISION REC?                   
         BNE   RREC200                                                          
*                                                                               
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2                                                        
         OC    NBRKKBUY,NBRKKBUY                                                
         BNZ   RREC1RSQ                                                         
*                                                                               
         GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA2                                                      
         USING NBRKEY,R2                                                        
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
RREC110  DS    0H                                                               
***  AFG  ***                                                                   
         TM    LFLAG,LAFGSFRM      WE DOING AFG+SPECIAL FORMAT?                 
         BO    RREC130                                                          
***  AFG  ***                                                                   
         CLI   INOFRM,C'A'        WANT ALL DAYPARTS?                            
         BNE   RREC120            NO - ONLY WANT DPT REQUESTED                  
         CLI   BDPT,0             ALL DPTS?                                     
         BE    RREC119            YES                                           
         LA    RE,SUBS            YES - WANT DPT REQUESTED &                    
         LA    RF,L'SUBS          ANY OTHER CORRESPONDING SUB-                  
RREC115  CLC   NBRSDYPT,0(RE)     DAYPARTS (SUBS DATA FROM VALDPL)              
         BE    RREC119                                                          
         CLI   NBRSSBDP,0         IF THERE IS A SUB-DPT                         
         BE    RREC118                                                          
         CLC   NBRSSBDP,0(RE)     AND IT MATCHES REQUESTED DPT                  
         BE    RREC119            DISPLAY RECORD INFO                           
RREC118  LA    RE,1(RE)                                                         
         BCT   RF,RREC115                                                       
         B     RREC1RSQ           NOT DPT OR MATCHING SUB-DPT (DO SEQ)          
RREC119  DS    0H                                                               
         CLI   NBRSSBDP,0         IF THERE IS A SUB- DAYPART                    
         BE    *+10               SET IT TEMP TO DAYPART FOR ROUTINES           
         MVC   NBRSDYPT,NBRSSBDP    IN THE 30 PGM                               
         B     RREC125                                                          
*                                                                               
RREC120  CLI   BDPT,0             ALL DPTS?                                     
         BE    RREC125            YES                                           
         CLC   NBRSDYPT,BDPT      NOPE -CHECK DAYPART REQUESTED                 
         BNE   RREC1RSQ           NOT A MATCH - DO SEQUENTIAL                   
*                                                                               
RREC125  CLI   BSLN,0              ALL SPOT LENGTHS?                            
         BE    RREC130             YES                                          
         CLC   NBRSSLN,BSLN                                                     
         BNE   RREC1RSQ                                                         
*                                                                               
RREC130  BAS   RE,GETRTG          GET RATINGS                                   
* CALL DROOL W/ SAME REC FOR COST1 (, & COST2 & COST3 IF ANY)                   
         MVI   COSTIND,1          DEFAULT TO BWDCOST1                           
*                                                                               
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    *+12                                                             
         MVI   COSTIND,2                                                        
         B     RREC132                                                          
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    *+8                                                              
         MVI   COSTIND,3                                                        
*                                                                               
* IF ALL DAYPART REQUEST MUST BUILD TABLE - OF DAYPARTS ACTUALLY                
* READ FOR LATER USE BY THE GOAL ROUTINE - SAME FOR SPOT LENGTH                 
*                                                                               
RREC132  DS    0H                                                               
***  AFG  ***                                                                   
         TM    LFLAG,LAFGSFRM      WE DOING AFG+SPECIAL FORMAT?                 
         BO    RREC143                                                          
***  AFG  ***                                                                   
         CLI   INOFRM,C'A'        ALL DAYPART REQUEST?                          
         BNE   RREC138                                                          
         CLI   BDPT,0             IF SINGLE DPT - RELEVANT                      
         BNE   RREC139            DAYPARTS ALREADY ADDED TO TABLE               
         MVC   TMPDPT,NBRSDYPT                                                  
         BAS   RE,GETSUBS         GET SUB-DAYPARTS FOR THIS DAYPART             
         BNE   RREC1RSQ                                                         
         CLI   SUBTYPE,C'R'       REGULAR DAYPART ?                             
         BNE   RREC136                                                          
         MVC   TMPDPT,NBRSDYPT                                                  
         BRAS  RE,ADDPT                                                         
         B     RREC139                                                          
RREC136  LA    R1,SUBS                                                          
         LA    R0,L'SUBS                                                        
RREC137  CLI   0(R1),0                                                          
         BE    RREC139                                                          
         MVC   TMPDPT,0(R1)                                                     
         BRAS  RE,ADDPT                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,RREC137                                                       
         B     RREC139                                                          
*                                                                               
RREC138  CLI   BDPT,0             ALL DAYPART REQUEST?                          
         BNE   RREC139            NO - SINGLE DAYPART DON'T BUILD LIST          
         MVC   TMPDPT,NBRSDYPT                                                  
         BRAS  RE,ADDPT                                                         
*                                                                               
RREC139  CLI   BSLN,0             ALL SPOT LENGTH REQUEST?                      
         BNE   RREC143            ONE SPOT LENGTH - DON'T BUILD LIST            
*                                                                               
         CLI   NBRSSLN,X'FF'       X'FF' FOR DAYPART TOTAL?                     
         BE    RREC143             YES, DON'T INCLUDE IN COMSLLST               
*                                                                               
         LA    R1,COMSLLST                                                      
         LA    R0,L'COMSLLST                                                    
RREC142  CLI   0(R1),0            IF SPOT LENGTH NOT                            
         BNE   *+14               IN LIST                                       
         MVC   0(1,R1),NBRSSLN    ADD IT                                        
         B     RREC143            IF                                            
         CLC   NBRSSLN,0(R1)      ALREADY THERE                                 
         BE    RREC143            DON'T ADD IT AGAIN                            
         LA    R1,1(R1)                                                         
         BCT   R0,RREC142                                                       
         DC    H'0'                                                             
*                                                                               
RREC143  BAS   RE,GETSPTS         GET SPOTS INTO TABLE(CNT PNTS AS              
*                                 ( IF NOT WEEKLY)                              
         LA    RE,0                                                             
RREC144  STC   RE,WEEKNO          START DISPLAYING AT PERIOD START              
         CLC   WEEKNO,PERDISPS                                                  
         BNL   *+12                                                             
         LA    RE,1(RE)                                                         
         B     RREC144                                                          
*                                                                               
         BAS   RE,REDOSPT         SETS GDTLCST FOR SYSDRIVER                    
         CLI   DAILY,C'Y'         ARE WE DAILY?                                 
         BNE   RREC145                                                          
         BAS   RE,FDYINWK         GET DISPLACEMENT INTO WEEK                    
         BNE   RREC1RSQ           DON'T WANT IT                                 
         BAS   RE,CHKPER          CHECK WITHIN REQUESTED PERIOD                 
         BNE   RREC155            TRY NEXT WEEK                                 
*                                                                               
RREC145  B     RDEXIT             EXITING WILL CALL DROOL FOR INPUT             
*                                                                               
*                                                                               
*                                                                               
RREC147  DS    0H                 SEES IF ANY MORE RECORDS FOR DROOL            
*  FOR WEEKLY FORMAT - CALL DROOL W/ THE SAME REC FOR EACH WK IN CMP            
RREC155  XR    RE,RE                                                            
         IC    RE,WEEKNO                                                        
         LA    RE,1(RE)                                                         
         STC   RE,WEEKNO                                                        
         CLC   WEEKNO,PERDISPE    MAKE SURE NOT PAST PERIOD END                 
         BH    RREC160            IT IS                                         
         BAS   RE,REDOSPT                                                       
         CLI   DAILY,C'Y'         ARE WE DAILY?                                 
         BNE   RREC145                                                          
         BAS   RE,FDYINWK         GET DISPLACEMENT INTO WEEK                    
         BNE   RREC1RSQ           DON'T WANT IT                                 
         BAS   RE,CHKPER          CHECK IF IN RQUESTED PERIOD                   
         BNE   RREC155                                                          
         B     RREC145                                                          
*                                                                               
RREC160  CLI   COSTIND,1          SEE IF THERES ANY MORE RECORDS                
         BNE   RREC170                                                          
         CLC   INOFRM,=C'PT'       F=DPT?                                       
         BNE   RREC1RSQ                                                         
         CLI   NBRSSLN,X'FF'       ALREADY ADDED THIS REC TO DPT-255?           
         BNE   *+12                                                             
         MVI   GLMAXTLV,0         TOTAL UP EVERYTHING                           
         B     RREC1RSQ           YES, THEN GET THE NEXT RECORD                 
*                                                                               
*                                 DPT-LEN IS LEVEL 13, SO SETTING THIS          
*                                   SAYS DON'T PUT TOTAL OUT TO SORT IF         
         MVI   GLMAXTLV,14          LEVEL IS LESS THAN THIS                     
         MVI   NBRSSLN,X'FF'                                                    
         B     RREC130            LET DROOL SEE THIS FOR INPUT AS WELL          
*                                                                               
*                                                                               
RREC170  CLI   COSTIND,2                                                        
         BNE   RREC180                                                          
         MVI   COSTIND,1                                                        
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    RREC143                                                          
         MVI   COSTIND,3                                                        
         B     RREC143                                                          
RREC180  CLI   COSTIND,3                                                        
         BE    *+6                                                              
         DC    H'0'               ONLY POSSIBILITIES ARE 1,2,OR 3               
         MVI   COSTIND,1                                                        
         B     RREC143                                                          
*                                                                               
RREC200  DS    0H                                                               
NOMORE   MVI   ENDSW,X'FF'        MARK NO MORE RECORDS                          
RDEXIT   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADDS A DAYPART TO THE DAYPART LIST IF NOT ALREADY IN THE LIST                 
***********************************************************************         
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
GETSBNO  B     EXITNO                                                           
GETSBY   B     EXITYES                                                          
         EJECT                                                                  
***********************************************************************         
* GETS RATINGS FOR REQUESTED DEMOS CATEGORIES                                   
***********************************************************************         
GETRTG   NTR1                                                                   
         XC    SVFRTG,SVFRTG                                                    
         XC    SV2RTG,SV2RTG                                                    
         L     R3,AIOAREA2                                                      
         USING NBRRECD,R3                                                       
GTRTG60  LA    R1,NBRFSTEL                                                      
GTRTG65  CLI   0(R1),0            END OF RECORD                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),NBRDMELQ     DEMO ELEMENT                                  
         BE    GTRTG70                                                          
         XR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GTRTG65                                                          
*                                                                               
GTRTG70  DS    0H                                                               
***  2 DECIMAL                                                                  
         ST    R1,APPARM                                                        
         MVI   APPARM,C'B'         WE HAVE BUY REVISION RECORD                  
         GOTO1 AADJDEMO,APPARM                                                  
         MVI   APPARM,X'00'        TAKE OFF THE C'B'                            
         L     R1,APPARM           LOAD BACK R1                                 
***  2 DECIMAL                                                                  
         LA    R0,DEMNUM          CLEAR OUT RATINGS                             
         LA    R8,LDEMHLD+2                                                     
GTRTG75  XC    0(4,R8),0(R8)                                                    
         LA    R8,6(R8)                                                         
         BCT   R0,GTRTG75                                                       
*                                 GET NEW RATINGS                               
         LA    R0,DEMNUM          # OF DEMOS                                    
         LA    R8,LDEMHLD         SAVE DEMO AREA                                
         LR    R2,R1              SAVE BEGINNING                                
GTRTG80  LA    RE,L'DMODEMO       LENGTH TO BUMP                                
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,NBRDMDMO-NBRDMELD(R1) POINT TO DEMO                           
         CLC   1(2,R1),0(R8)                                                    
         BE    GTRTG90                                                          
         BXLE  R1,RE,*-10                                                       
         B     GTRTG95            THAT DEMO NOT IN RECORD - CHECK NEXT          
*                                                                               
GTRTG90  MVC   2(4,R8),4(R1)                                                    
***  2 DECIMAL                                                                  
         OC    3(3,R8),3(R8)       IS IT ZERO RATINGS?                          
         BNZ   GTRTG90C             - NOPE                                      
         TM    2(R8),X'80'         IS IT AN OVERRIDE?                           
         BO    GTRTG90C             - YUP IT IS                                 
         NI    2(R8),X'FF'-X'40'    - NOPE, TAKE OFF 2 DECIMAL BIT\             
***  2 DECIMAL                                                                  
*                                                                               
GTRTG90C C     R0,=F'6'           FIRST RATING?                                 
         BNE   GTRTG90E                                                         
         MVC   SVFRTG,5(R1)       SAVE IT                                       
GTRTG90E C     R0,=F'5'           2ND RATING?                                   
         BNE   GTRTG95                                                          
         MVC   SV2RTG,5(R1)       SAVE IT TOO                                   
*                                                                               
GTRTG95  LA    R8,6(R8)                                                         
         LR    R1,R2                                                            
         BCT   R0,GTRTG80                                                       
*                                                                               
GTRTGX   B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GETSPTS - SETS UP SPOTABLE - ARRAY OF SPOTS PER WEEK(14X1)                    
*           IT ALSO SETS GDTLCST - TOTAL SPOT COUNT FOR CAMPAIGN                
***********************************************************************         
GETSPTS  NTR1                                                                   
         XC    SPOTABLE,SPOTABLE                                                
         XC    GDTLCST,GDTLCST                                                  
*                                                                               
         L     R3,AIOAREA2                                                      
         USING NBRKEY,R3                                                        
GETSPT00 LA    R8,NBRFSTEL                                                      
         LR    R3,R8                                                            
         USING NBRSELD,R3                                                       
*                                                                               
         TM    MISCFLG1,MF1COSOV   ARE WE UPTO COST OVERRIDES?                  
         BNZ   GTSPT200            YES, GET TO IT THEN                          
*                                                                               
GETSPT05 CLI   0(R8),0            END OF RECORD?                                
         BE    GETSPTX                                                          
         CLI   0(R8),NBRSPELQ     SPOTS PER WEEK ELEMENT?                       
         BE    GETSPT07                                                         
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     GETSPT05           CHECK NEXT ELEMENT                            
*                                                                               
GETSPT07 XR    RE,RE                                                            
         IC    RE,1(R8)                                                         
         SHI   RE,NBRSPSPW-NBRSPELD+1    RE CONTAINS LENGTH OF TABLE            
         LA    R8,NBRSPSPW-NBRSPEL(R8)   POINT TO START OF TABLE                
         EX    RE,GETSPT08                                                      
         B     *+10                                                             
GETSPT08 MVC   SPOTABLE(0),0(R8)                                                
*                                                                               
***      TM    LIND,LPOL           POL BUYS?                                    
***      BZ    GETSPT09            NO, CAN'T HAVE COST OVERRIDES                
         L     R1,AIOAREA2                                                      
         USING NBRKEY,R1                                                        
         OC    NBRKKBUY,NBRKKBUY   MANUALLY ADDED?                              
         BNZ   GTSPT100            NO, WE CAN'T HAVE EFFECTIVE DATES            
         DROP  R1                                                               
*                                                                               
GETSPT09 XC    APDUB,APDUB        USE FOR DATE                                  
         LR    R2,R5              CAMPAIGN DATES                                
         AHI   R2,CMPDATSD-TWAD                                                 
         ZIC   R8,CMPNWKS         MAX NUM OF DAYS/WEEKS WE'RE HOLDING           
         CLI   COSTIND,1                                                        
         BNE   GETSPT50                                                         
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    GTSPT300           ONLY ONE EFFECTIVE COST                       
         GOTO1 VDATCON,APPARM,(3,NBRSEDT2),(0,APDUB)                            
         BAS   RE,GETBEG          FINDS BEGINNING OF WEEK FOR APDUB             
         LA    RE,SPOTABLE                                                      
GETSPT10 CLC   0(6,R2),APDUB                                                    
         BNL   GETSPT20                                                         
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT10                                                      
         DC    H'0'                                                             
*                                                                               
GETSPT20 MVI   0(RE),0            ZERO OUT REST                                 
         LA    RE,1(RE)                                                         
         BCT   R8,GETSPT20                                                      
         B     GTSPT300                                                         
         SPACE 2                                                                
GETSPT50 CLI   COSTIND,2                                                        
         BNE   GETSPT80                                                         
         GOTO1 VDATCON,APPARM,(3,NBRSEDT2),(0,APDUB)                            
         BAS   RE,GETBEG                                                        
         MVC   APWORK+6(6),APDUB  SAVE TEMPORARILY                              
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    GETSPT52           GOOD TILL END OF TABLE                        
         GOTO1 VDATCON,APPARM,(3,NBRSEDT3),(0,APDUB)                            
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
         DC    H'0'                                                             
*                                                                               
GETSPT55 OC    NBRSEDT3,NBRSEDT3                                                
         BZ    GTSPT300           GOOD TILL END OF TABLE                        
GETSPT60 CLC   0(6,R2),APWORK                                                   
         BNL   GETSPT65                                                         
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT60                                                      
         DC    H'0'                                                             
*                                                                               
GETSPT65 MVI   0(RE),0            ZERO OUT REST OF TABLE                        
         LA    RE,1(RE)                                                         
         BCT   R8,GETSPT65                                                      
         B     GTSPT300                                                         
*                                                                               
GETSPT80 CLI   COSTIND,3                                                        
         BE    GETSPT83                                                         
         DC    H'0'                                                             
*                                                                               
GETSPT83 GOTO1 VDATCON,APPARM,(3,NBRSEDT3),(0,APDUB)                            
         BAS   RE,GETBEG                                                        
         LA    RE,SPOTABLE                                                      
GETSPT85 CLC   0(6,R2),APDUB                                                    
         BNL   GTSPT300                                                         
         MVI   0(RE),0                                                          
         LA    RE,1(RE)                                                         
         LA    R2,6(R2)                                                         
         BCT   R8,GETSPT85                                                      
         B     GTSPT300                                                         
         EJECT                                                                  
***********************************                                             
* POL BUY REVISION THAT IS LINKED                                               
***********************************                                             
GTSPT100 XC    NBRCSTBL,NBRCSTBL   REMOVE ANY COST OVERRIDES                    
         MVI   NBRCSPTR,0                                                       
         MVI   NBRCSNUM,0                                                       
         XR    R2,R2                                                            
         IC    R2,CMPNWKS          R2 = # OF WEEKS IN THIS CAMPAIGN             
         L     RE,ATWA             RE = A(FIRST WEEK IN THE CAMPAIGN)           
         AHI   RE,CMPDATSP-TWAD    RE = A(FIRST WEEK IN THE CAMPAIGN)           
         LA    R8,SPOTABLE         R8 = A(# OF SPOTS FOR THE WEEK)              
*                                                                               
GTSPT110 LR    R1,R3               R1 = A(1ST ELEMENT IN REVISION REC)          
GTSPT111 CLI   0(R1),0             ANY MORE OVERRIDES FOR THIS WEEK?            
         BE    GTSPT190            NONE, CHECK THE NEXT WEEK                    
         CLI   0(R1),NBRCOELQ                                                   
         BE    GTSPT120                                                         
GTSPT115 XR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GTSPT111                                                         
*                                                                               
         USING NBRCOELD,R1                                                      
GTSPT120 CLC   NBRCODAT,0(RE)      FIND AN OVERRIDE FOR THIS WEEK?              
         BL    GTSPT115            NO, CHECK NEXT OVERRIDE                      
         CLC   NBRCODAT,2(RE)                                                   
         BH    GTSPT190            NO, WE'RE BEYOND THIS WEEK                   
*                                                                               
         LA    R6,NBRCSTBL                                                      
         CLI   NBRCSNUM,0          ANY OVERRIDES YET?                           
         BE    GTSPT130            NONE, WE HAVE OUR 1ST FOR THIS REC           
         XR    RF,RF                                                            
         IC    RF,NBRCSNUM         HAVE THIS OVERRIDE ALREADY?                  
GTSPT125 CLC   NBRCOCST,0(R6)                                                   
         BE    GTSPT135            YES, JUST DECREMENT # SPOTS                  
         LA    R6,4(R6)                                                         
         BCT   RF,GTSPT125                                                      
*                                                                               
GTSPT130 XR    RF,RF               INCREMENT NUMBER OF COST OVERRIDES           
         IC    RF,NBRCSNUM                                                      
         LA    RF,1(RF)                                                         
         STC   RF,NBRCSNUM                                                      
GTSPT135 MVC   0(L'NBRCOCST,R6),NBRCOCST                                        
         CLI   0(R8),0             DECREMENT # OF SPOTS IN WEEK                 
         BE    GTSPT115                                                         
         IC    RF,0(R8)                                                         
         BCTR  RF,0                                                             
         STC   RF,0(R8)                                                         
         B     GTSPT115                                                         
*                                                                               
GTSPT190 LA    R8,1(R8)            R8 = NEXT A(# OF SPOTS)                      
         LA    RE,4(RE)            RE = NEXT A(WEEK IN CAMPAIGN)                
         BCT   R2,GTSPT110         GO THROUGH ALL THE WEEKS IN CAMPAIGN         
         B     GTSPT300                                                         
         EJECT                                                                  
***************                                                                 
* FILLING IN SPOTABLE WITH # OF SPOTS FOR EACH OVERRIDE                         
***************                                                                 
GTSPT200 XR    RF,RF               BUMP UP TO THE NTH OVERRIDE                  
         IC    RF,NBRCSPTR                                                      
         LR    R6,RF                                                            
         LA    RF,1(RF)                                                         
         STC   RF,NBRCSPTR                                                      
         CLM   RF,1,NBRCSNUM                                                    
         BNH   *+6                                                              
         DC    H'0'                                                             
         MHI   R6,L'NBRCOCST                                                    
         LA    R6,NBRCSTBL(R6)     R6 = A(OVERRIDE COST WE WANT)                
*                                                                               
*                                  FAKE OUT DROOL INTO THINKING THIS            
         MVC   NBRSCST1,0(R6)         OVERRIDE COST IS OUR DEFAULT COST         
*                                                                               
         XR    R2,R2                                                            
         IC    R2,CMPNWKS          R2 = # OF WEEKS IN THIS CAMPAIGN             
         LR    RE,R5               RE = A(FIRST WEEK IN THE CAMPAIGN)           
         AHI   RE,CMPDATSP-TWAD    RE = A(FIRST WEEK IN THE CAMPAIGN)           
         LA    R8,SPOTABLE         R8 = A(# OF SPOTS FOR THE WEEK)              
*                                                                               
GTSPT210 LR    R1,R3               R1 = A(1ST ELEMENT IN REVISION REC)          
GTSPT211 CLI   0(R1),0             ANY MORE OVERRIDES FOR THIS WEEK?            
         BE    GTSPT290            NONE, CHECK THE NEXT WEEK                    
         CLI   0(R1),NBRCOELQ                                                   
         BE    GTSPT220                                                         
GTSPT215 XR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GTSPT211                                                         
*                                                                               
         USING NBRCOELD,R1                                                      
GTSPT220 CLC   NBRCODAT,0(RE)      FIND AN OVERRIDE FOR THIS WEEK?              
         BL    GTSPT215            NO, CHECK NEXT OVERRIDE                      
         CLC   NBRCODAT,2(RE)                                                   
         BH    GTSPT290            NO, WE'RE BEYOND THIS WEEK                   
         CLC   NBRCOCST,0(R6)                                                   
         BNE   GTSPT215                                                         
*                                                                               
         CLI   NBRCOLEN,NBRCOL2Q   IS IT NEW LENGTH?  (WITH NEW STATUS)         
         BNE   GTSPT250             - NOPE, WE NEED TO INCREMENT                
         TM    NBRCOFLG,NBRCOMIN+NBRCOMNS   X'C0' WE HAVE MINUS STUFF??         
         BNZ   GTSPT215             - YES WE DO, DON'T COUNT SPOT               
*                                                                               
GTSPT250 IC    RF,0(R8)            INCREMENT # OF SPOTS FOR THIS COST           
         LA    RF,1(RF)                                                         
         STC   RF,0(R8)                                                         
         B     GTSPT215                                                         
*                                                                               
GTSPT290 LA    R8,1(R8)            R8 = NEXT A(# OF SPOTS)                      
         LA    RE,4(RE)            RE = NEXT A(WEEK IN CAMPAIGN)                
         BCT   R2,GTSPT210         GO THROUGH ALL THE WEEKS IN CAMPAIGN         
         B     GTSPT300                                                         
*                                                                               
GTSPT300 LA    R8,SPOTABLE                                                      
         ZIC   R0,CMPNWKS                                                       
         XR    R1,R1                                                            
         XR    RE,RE                                                            
GTSPT310 IC    RE,0(R8)                                                         
         AR    R1,RE                                                            
         LA    R8,1(R8)                                                         
         BCT   R0,GTSPT310                                                      
         STH   R1,GDTLCST         SAVE NUMBER OF SPOTS                          
*                                                                               
GETSPTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FINDS THE BEGINNING DAY OF THE WEEK THAT APDUB IS IN                          
***********************************************************************         
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
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REDOSPT - RECALCULATES GDTLCST TO BE THE SPOT COUNT                           
*           FOR THE CURRENT WEEK                                                
***********************************************************************         
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
         MVC   APBYTE,NBRSDAYS    IN CASE NOT PACKAGE/ORBIT RECORD              
*&&DO                                                                           
         TM    NBRSINDS,NBRSIORB  ORBIT RECORD?                                 
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
FDY10    TM    NBRSINDS,NBRSIPKG  PACKAGE RECORD?                               
         BZ    FDY30              NO- USE BWDDAYS                               
         CLI   BWDKELSQ,0         PACKAGE RECORD AND MASTER                     
         BE    FDYNO              WE ARE NOT INTERESTED                         
         MVC   APBYTE,BWDPODAY    OTHERWISE MUST BE SLAVE - USE IT              
*&&                                                                             
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
         B     EXITYES                                                          
*                                  MUST BE PACKAGE MASTER                       
FDYNO    B     EXITNO              OR ORBIT SLAVE - DON'T WANT IT               
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
CHKPER15 B     EXITYES            IT IS IN REQUESTED PERIOD                     
*                                                                               
CHKPER20 B     EXITNO             NOT IN REQUESTED PERIOD                       
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
         BNL   EXITYES                                                          
         B     EXITNO                                                           
         EJECT                                                                  
*===========================================================*                   
* OUTPUT - CALLED BEFORE DRIVER/DROOL IS CALLED FOR OUTPUT  *                   
*===========================================================*                   
OUTPUT   DS    0H                                                               
         B     EXIT                                                             
         SPACE                                                                  
*=======================*                                                       
* DRIVER HOOK - HEADHK  *                                                       
*=======================*                                                       
*                                                                               
DRHOOK   L     R9,AREP                                                          
         USING REPD,R9                                                          
         L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
*                                                                               
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
         DROP  R4,R9                                                            
         EJECT                                                                  
EXITYES  SR    RC,RC                                                            
EXITNO   LTR   RC,RC                                                            
EXIT     XIT1  ,                                                                
*==================*                                                            
* LITERAL POOL     *                                                            
*==================*                                                            
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
DPTSTXO  CR    R1,RE               IS IT UP TO THE NEXT TABLE YET?              
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
         SR    R2,R2              CALC PERIOD START & END                       
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
***********************************************************************         
* READS GOAL RECORDS FOR EACH DPT/SLN FOR PTS & DOLS FOR EACH MARKET            
***********************************************************************         
GOALS    NTR1  BASE=*,LABEL=*                                                   
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
         AHI   R4,L'SVWORK-1     TRY TO STICK IN '$' DAYPART                    
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
         L     RE,AGLOBAL              SO DRIVER WON'T INCLUDE THIS             
         MVI   GLMAXTLV-GLOBALD(RE),14   SUBTOTALS AGAIN IN TOTALS              
         L     R1,AIOAREA3                                                      
         MVC   0(4,R1),SUBGDOL                                                  
         MVC   4(4,R1),SUBGPNT                                                  
         B     EXIT                CALL DRIVER FOR INPUT (DPT-255)              
*****                                                                           
GOAL18A  DS    0H                                                               
         L     RE,AGLOBAL                                                       
         MVI   GLMAXTLV-GLOBALD(RE),0   TOTAL UP EVERYTHING                     
*                                                                               
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
         BZ    GOAL21A9                                                         
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
         LA    RF,220(R1)                                                       
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
*                                                                               
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
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1FSTTM EQU   X'80'                - FIRST TIME THRU DETAIL INPUT              
MF1MANUL EQU   X'40'                - UPTO MANUALLY ADDED BUYS                  
MF1COSOV EQU   X'20'                - LOOK FOR A COST OVERRIDE NOW              
*                                                                               
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
*OMSLLST DS    CL(SLNTABLQ)       EXPANDED 4 ALL POSSIBLE LENGTHS (16)          
COMSLLST DS    CL255              SLNTAB NOW CORERES WITH ALL SPOT LENS         
COMDPLST DS    CL36               DAYPART LIST FOR GETTING GOALS                
SVWORK   DS    CL(L'COMDPLST)     SAVED DAYPART LIST                            
*                                                                               
TMPDPT   DS    CL1                TEMP. DAYPART                                 
SUBS     DS    CL17               SAVED SUB-DAYPART LIST                        
SUBTYPE  DS    CL1                DAYPART TYPE FROM GETSUB                      
*                                                                               
SVBUYKEY DS    XL(L'BUYKEY)        SAVED BUY KEY                                
*                                                                               
MAXCOSTS EQU   8                                                                
NBRCSNUM DS    XL1                 NUMBER OF COST OVERRIDES                     
NBRCSPTR DS    XL1                 DISP CPUNTER OF CURRENT OVERRIDE             
NBRCSTBL DS    0XL(MAXCOSTS*L'NBRCOCST)  TABLE OF OVERRIDE COSTS                
CURRCOST DS    XL4                 CURRENT OVERRIDE COST                        
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
* SPNWSBRV                                                                      
* SPGENDAYPT                                                                    
* SPGENBUY                                                                      
* DRGLOBAL AND DROOLLOCAL                                                       
* FAFACTS                                                                       
* FATIOB                                                                        
* SPDEMUPD                                                                      
* DDSCANBLKD                                                                    
       ++INCLUDE SPNWSHDR                                                       
       ++INCLUDE SPNWSCAM                                                       
       ++INCLUDE SPNWSBRV                                                       
       ++INCLUDE SPGENDAYPT                                                     
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DROOLLOCAL                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPDEMUPD                                                       
       ++INCLUDE DDSCANBLKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'119SPNWS33   02/27/07'                                      
         END                                                                    
