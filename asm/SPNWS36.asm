*          DATA SET SPNWS36    AT LEVEL 135 AS OF 08/25/08                      
*PHASE T20736A,*                                                                
         TITLE 'NWS36 - BUYERS WORK SHEET - BUY MAINT OVERLAY'                  
T20736   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20736**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    R6,LASAVE                                                        
         ST    RE,APRELO                                                        
         ST    RB,APNTRYA                                                       
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LA    R2,IOKEY            R2=A(CAMPAIGN MARKET HEADER KEY)             
         USING BWHRECD,R2                                                       
         LA    R3,APRECKEY         R3=A(CAMPAIGN MARKET DETAIL KEY)             
         USING BWDRECD,R3                                                       
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
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     DELREC                                                           
         B     EXIT    RESREC                                                   
         B     VALPAR                                                           
         B     GETSEL                                                           
         B     DISSEL                                                           
         B     VALSEL                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     FSTSCR                                                           
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     DISSEL                                                           
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
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VALKEY   TM    LINDS2,L2XFRDTL                                                  
         BZ    VALK1                                                            
         MVC   FVMSGNO,=H'801'                                                  
         LA    R1,REVMEDH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
*                                                                               
VALK1    GOTO1 AVALMED,REVMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALBYR,REVBYRH     VALIDATE BUYER                               
         BNE   VALKX                                                            
*                                                                               
         OC    BYRPW,BYRPW         BUYER PASSWORD                               
         BZ    VALK2                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALKX                                                            
*                                                                               
VALK2    GOTO1 AVALCAM,REVNUMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AGETCLT,CMPCLTC     (GET CLIENT)                                 
         BE    *+16                                                             
         LA    R1,REVNUMH                                                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
         BRAS  RE,COS2CHK          CHECK FOR COS2!!                             
*                                                                               
         GOTO1 AGETPRD,CMPPRDN     (GET PRODUCT)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R8,ESTDEMS          FORMAT DEMO NAMES TO SCREEN                  
         BAS   RE,FMTDEMS                                                       
*                                                                               
         GOTO1 AVALSTA,REVSTAH     VALIDATE STATION                             
         BNE   VALKX                                                            
*                                                                               
         XC    LESTREP,LESTREP                                                  
****  NEED TO CHECK THE ESTREP FIELD TO SEE IF WE HAVE DEFAULT                  
         OC    ESTREP,ESTREP       ANYTHING?                                    
         BZ    VALK2D               - NOPE                                      
*     WE NEED LESTREP FILLED EVEN IF REVREPN HAS SOMETHING!!                    
*                                                                               
         GOTO1 VCOLY,APPARM,0,X'D9000ABC'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),APPARM,(C'U',ESTREP),LESTREP                                
****                                MHC  06/10/04                               
         OC    REVREPN,REVREPN     SOMETHING IN DETAIL SCREEN FOR REP?          
         BNZ   VALK2C               - YUP!                                      
*                                                                               
****  CODE FOR THE NEW REP FIELD ON THE SCREEN                                  
         MVC   REVREPN,LESTREP                                                  
VALK2C   MVI   REVREPNH+5,3                                                     
         OI    REVREPNH+6,X'80'                                                 
****                                 MHC  06/10/04                              
******                                                                          
VALK2D   LA    R2,IOKEY            BUILD HEADER POINTER TO FIND                 
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
         LA    R1,INPTTABL         UNPROTECT ALL INPUT FIELDS                   
VALK2E   SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         BZ    VALK2G                                                           
         AR    RE,R5                                                            
         NI    1(RE),X'FF'-X'20'                                                
         OI    6(RE),X'80'         TRANSMIT                                     
         LA    R1,2(R1)                                                         
         B     VALK2E                                                           
***********************************                                             
* SET THE BUY LINE NUMBER                                                       
***********************************                                             
VALK2G   LA    R2,REVLINH          VALIDATE LINE NUMBER                         
         ST    R2,FVADDR           SO WE POINT TO CORRECT FIELD ON ERR          
         CLI   5(R2),0                                                          
         BNE   VALK2N                                                           
         CLI   APACTN,ACTADD       ADDING?                                      
         BNE   VALK3                - NOPE, WILL GO TO VALK3M SOON              
***                                ...MUST BE FROM WORK SKED (>) !!             
******  CHECK IF WE'RE DOING C'=' DUPING!!                                      
         TM    TWAMODE,TWAMLSM     AND IN LIST/SELECT MODE                      
         BNO   VALK3                - NOPE, WE NEED AN AUTO NUMBER              
*                                                                               
****  WE NEED TO CLEAR LDEMVALS LEFT OVER FROM 05, 35                           
         XC    LDEMVALS(LDEMOLD-LDEMVALS),LDEMVALS                              
****                            MHC   01/14/05                                  
*                                                                               
         CLI   REVSEQH+5,0                                                      
         BNE   VALK2I                                                           
         OI    LINDS2,L2FRMWRK     NO REVLIN OR REVSEQ + ADD...                 
         B     VALK3               ...MUST BE C'>' FROM WORK SKED               
*                                                                               
VALK2I   LA    R1,REVSEQH                                                       
         ST    R1,FVADDR                                                        
         ZIC   R1,REVSEQH+5         - YUP, WE NEED TO GRAB SEQNUM...            
         BCTR  R1,0                                                             
         EX    R1,*+8               ... FROM THE SCREEN FOR DISPLAY             
         B     *+10                                                             
         PACK  APDUB,REVSEQ(0)      ... AT VALK40 LATER ON                      
         CVB   R1,APDUB                                                         
         CHI   R1,256                                                           
         BH    EIIF                                                             
         LTR   R1,R1                                                            
         BZ    EIIF                                                             
         STC   R1,BSEQNUM2         SAVE OFF ORIGINAL BINARY SEQNUM              
******                               MHC  09/04/03                              
         B     VALK3                - YEAH, WE WON'T HAVE A BBUYLINE            
*                                                                               
VALK2L   LR    R1,R2               SET R1 TO A(FIELD HDR) FOR ENOC              
         B     ENOC                MISSING INPUT FILED                          
*                                                                               
VALK2N   TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    EIIF                NO                                           
         TM    TWAMODE,TWAMLSM     ADDING, IS IT IN LIST/SELECT MODE??          
         BO    VALK2P               - YUP, ADDING IS OK                         
*                                                                               
         CLI   APACTN,ACTADD       ADDING                                       
         BE    EIIF                NO GOOD, THEY SHOULD USE SEQ                 
*                                                                               
*ALK2O   ZIC   R1,REVSEQH+5         - YUP, WE NEED TO GRAB SEQNUM...            
*        BCTR  R1,0                                                             
*        EX    R1,*+8               ... FROM THE SCREEN FOR DISPLAY             
*        B     *+10                                                             
*        PACK  APDUB,REVSEQ(0)                                                  
*        CVB   R1,APDUB                                                         
*        CHI   R1,256                                                           
*        BH    EIIF                                                             
*        LTR   R1,R1                                                            
*        BZ    EIIF                                                             
*        STC   R1,BSEQNUM2         SAVE BINARY LINE NUMBER                      
********                               MHC  09/04/03                            
*                                                                               
VALK2P   ZIC   R1,REVLINH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,REVLIN(0)                                                  
         CVB   R1,APDUB                                                         
         CHI   R1,256                                                           
         BH    EIIF                                                             
         LTR   R1,R1                                                            
         BZ    EIIF                                                             
         STC   R1,BBUYLINE         SAVE BINARY LINE NUMBER                      
*                                                                               
****  IF IT'S C'=' DUPING....                                                   
         TM    TWAMODE,TWAMLSM     AND IN LIST/SELECT MODE                      
         BO    VALK3                - YUP, WE NEED TO SET SEQNUM                
****                                   MHC  09/04/03                            
         B     VALK4                                                            
***********************************                                             
* SET THE SEQUENCE NUMBER                                                       
***********************************                                             
VALK3    LA    R2,REVSEQH                                                       
         CLI   APACTN,ACTADD                                                    
         BNE   VALK3M                                                           
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         USING NBRKEY,R2                                                        
         MVI   NBRKTYP,NBRKTYPQ    READ NWS BUY REVISION RECORD                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,BSTA                                                     
*                                                                               
         GOTO1 AIO,DIRHI                                                        
VALK3A   CLC   IOKEY(NBRKNBSQ-NBRKEY),IOKEYSAV                                  
         BNE   VALK3B                                                           
         MVC   BSEQNUM,NBRKNBSQ                                                 
         GOTO1 AIO,DIRSQ                                                        
         B     VALK3A                                                           
         DROP  R2                                                               
*                                                                               
VALK3B   LA    R2,REVSEQH                                                       
         XR    R1,R1                                                            
         IC    R1,BSEQNUM                                                       
         LA    R1,1(R1)                                                         
         CHI   R1,255                                                           
         BH    EIIF                                                             
         STC   R1,BSEQNUM                                                       
         CVD   R1,APDUB                                                         
         UNPK  REVSEQ,APDUB                                                     
         OI    REVSEQ+2,X'F0'                                                   
         OI    REVSEQH+6,X'80'                                                  
         B     VALK4                                                            
***********************************                                             
* NOT ADDING THE NBR RECORD                                                     
***********************************                                             
VALK3M   CLI   REVSEQH+5,0         ANYTHING IN THE SEQUENCE FIELD?              
         BNE   VALK3N                                                           
         CLI   REVLINH+5,0                                                      
         BNE   VALK4               NEED AT LEAST LINE OR SEQ NUMBERS            
         LA    R1,REVSEQH                                                       
         B     ENOC                                                             
*                                                                               
VALK3N   CLI   REVLINH+5,0                                                      
         BNE   EIIF                CAN'T HAVE BOTH LINE AND SEQ NUMBERS         
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    EIIF                NO                                           
         ZIC   R1,REVSEQH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,REVSEQ(0)                                                  
         CVB   R1,APDUB                                                         
         CHI   R1,256                                                           
         BH    EIIF                                                             
         LTR   R1,R1                                                            
         BZ    EIIF                                                             
         STC   R1,BSEQNUM          SAVE BINARY LINE NUMBER                      
*                                                                               
VALK4    LA    R2,IOKEY                                                         
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
VALK4A   GOTO1 AIO,DIRHI                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(BUYKBUY-BUYKEY),IOKEYSAV                                   
         BNE   VALK99                                                           
*                                                                               
         CLI   BUYKBUY,0           REG, PB ACTV, OR POL BUY?                    
         BNE   VALK5               NO                                           
         CLC   BBUYLINE,BUYKBUY+1  YES, SAME LINE NUMBER?                       
         BNE   VALK99                                                           
         B     VALK10                                                           
*                                                                               
VALK5    CLI   BUYKBUY,X'FF'       POL-BRND?                                    
         BNE   VALK99                                                           
         CLC   BBUYLINE,BUYKBUY+2  YES, SAME LINE NUMBER?                       
         BE    VALK10                   YES                                     
         BL    VALK99                                                           
         MVC   BUYKBUY+2(1),BBUYLINE    NO, SET IT TO THE SAME LINE #           
         B     VALK4A                       AND TRY GETTING KEY NOW             
*                                                                               
VALK10   MVC   APRECKEY(20),IOKEY                                               
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         GOTO1 AIO,FILGET2                                                      
****  CHECK TO SEE IF IT'S C'=' DUPING FROM BUY SKED!!                          
         TM    TWAMODE,TWAMLSM     AND IN LIST/SELECT MODE                      
         BNO   VALKX                - NOPE, IT'S DONE WITH                      
         CLI   APACTN,ACTADD       ADDING?                                      
         BNE   VALKX                - NOPE, GET OUTTA HERE                      
         OI    APINDS,APIOKADD     OK TO ADD                                    
***  USING LTRT AS TEMP SPACE, THIS IS ONLY USED IN 1 OTHER PLACE!!             
         MVC   LTRT(3),REVSEQ      SAVE OFF REVSEQ FOR DISPLAY                  
         MVI   REVSEQH+5,0         CLEAR IT OUT CUZ WE GOING THROUGH...         
         XC    REVSEQ,REVSEQ       ...DISPRECD FIRST                            
*                                                                               
         BRAS  RE,DISPRECD         WE NEED TO DISPLAY IT FIRST                  
*                                                                               
         MVC   REVSEQ,LTRT         RESTORE THE SEQUENCE NUMBER                  
         MVI   REVSEQH+5,3         SHOULD BE 3 CHARS LONG                       
*                                                                               
         BRAS  RE,SETLNTH          SET LENGTHS FOR FIELDS ON SCREEN             
         XC    IOKEY,IOKEY                                                      
         L     R2,AIOAREA3         AFTER DISPRECD AIO3 HAS THE...               
         MVC   IOKEY(20),0(R2)     ...KEY THAT WE WANT                          
*                                                                               
         XC    REVLIN,REVLIN       CLEAR THE LINE NUMBER                        
         MVI   REVLINH+5,0         CLEAR INPUT NUMBER                           
*                                                                               
         USING NBRRECD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    NBRKKBUY,NBRKKBUY   NEED A CLEAN BUY DETAIL                      
         MVC   NBRKNBSQ,BSEQNUM    NEED TO UPDATE THE SEQUENCE NUMBER           
*                                                                               
         MVC   APRECKEY(20),IOKEY                                               
         B     VALKX                                                            
         DROP  R2                                                               
****  WE'RE DONE WITH NEW C'=' DUPING CODE NOW                                  
****                                        MHC  09/04/03                       
*                                                                               
*ALK15   OI    APINDS,APIOKADD     OK TO ADD                                    
VALK15   NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL-APIOKRES                    
*                                                                               
         CLI   APACTN,ACTADD       IS IT ADD?                                   
         BNE   *+8                                                              
         OI    APINDS,APIOKADD     OK TO ADD                                    
         TM    APINDS,APIOKADD     TEST RECORD NOT FOUND                        
         BZ    VALKX                                                            
         TM    TWAMODE,TWAMLSM     AND IN LIST/SELECT MODE                      
         BNO   VALKX                - NOPE, IT'S DONE WITH                      
         BRAS  RE,DISPRECD         WE NEED TO DISPLAY IT FIRST                  
         BRAS  RE,SETLNTH          SET LENGTHS FOR FIELDS ON SCREEN             
         B     VALKX                                                            
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
*                                                                               
         GOTO1 AIO,DIRHI                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BE    VALK10                                                           
         CLI   APACTN,ACTADD       IS IT ADD?                                   
         BNE   *+12                                                             
         OI    APINDS,APIOKADD     OK TO ADD                                    
         B     VALK45                                                           
*                                                                               
         TM    TWAMODE,TWAMLSM     IN LIST/SELECT MODE                          
         BZ    VALK45                                                           
*   SETTING THIS TO ALSO STOP THE DUMPS IN GEGEN00:LFM4                         
         OI    APINDS,APIOKDIS     YES, SHOULD STILL BE OKAY TO DISPLAY         
*                                                                               
VALK45   NI    APINDS,FF-APIOKCHA-APIOKDEL-APIOKRES                             
*                                                                               
         XC    NBRKEY,NBRKEY       SET UP KEY AGAIN FOR VALREC                  
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,BSTA                                                     
         MVC   NBRKNBSQ,BSEQNUM                                                 
*                                                                               
         TM    TWAMODE,TWAMLSM     IN LIST/SELECT MODE                          
         BZ    VALKX               NO                                           
*                                                                               
         CLI   APRECKEY,BWDKTYPQ   YES, FROM WORK/SKED?                         
         BNE   VALK99               - NO, RETURN ERROR CODE                     
*****  ARE IN DUPING FROM C'=' FROM BUY SKED OR C'>' FROM WORK SKED???          
         TM    TWAMODE,TWAMLSM     IN LIST/SELECT MODE                          
         BNO   VALK47               - NOPE, CONTINUE                            
         TM    LINDS2,L2FRMWRK     IS IT FROM WORK SKED??                       
         BNO   VALK50               - NOPE, FROM BUY SKED                       
*                                                                               
VALK47   CLI   APRECKEY+1,BWDKSUBQ                                              
         BE    VALKX               YES, OKAY TO ADD HERE                        
         B     VALK99              YES-RETURN ERROR CODE                        
*                                                                               
*****  WE ARE DUPING HERE!!                                                     
VALK50   CLI   APRECKEY+1,NBRKSTYQ   ARE WE DOING BUY REVISION?                 
         BNE   VALK99               - NO, RETURN ERROR CODE                     
         MVC   NBRKNBSQ,BSEQNUM2   REPLACE THE SEQNUM WITH BSEQNUM2             
*                                                                               
         BRAS  RE,DISPRECD         WE NEED TO DISPLAY IT FIRST                  
*                                                                               
         MVC   NBRKNBSQ,BSEQNUM    RESTORE                                      
         BRAS  RE,SETLNTH          SET LENGTHS FOR FIELDS ON SCREEN             
         B     VALKX                                                            
*&&DO                                                                           
         BE    *+12                                                             
         MVI   APINDS,APIOKADD                                                  
         B     *+12                                                             
         BRAS  RE,IOCHECK                                                       
         BNE   VALKX                                                            
         MVC   LHDRKEY,IOKEY       SAVE THE HEADER KEY                          
         MVC   FVMSGNO,=AL2(FVFOK) SET OK RETURN CODE                           
         MVI   LINDS,0                                                          
         CLI   APACTN,ACTADD       FOR ACTION NOT ADD -                         
         BE    VALK8                                                            
         TM    APINDS,APIOKADD     TEST FOR HEADER NOT FOUND                    
         BO    VALK30                                                           
         TM    APINDS,APIOKRES     TEST FOR HEADER DELETED                      
         BZ    VALK11                                                           
         NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL YES                         
         B     VALKX                                                            
*                                  ACTION ADD -                                 
VALK8    TM    APINDS,APIOKADD     TEST FOR HEADER NOT FOUND                    
         BZ    VALK10                                                           
         OI    LINDS,LNOHDR        INDICATE NO HEADER                           
         MVC   LHDRKEY,IOKEYSAV    SAVE ORIGINAL KEY                            
         XC    IOKEY,IOKEY         READ PASSIVE POINTERS TO GET NEXT            
         MVI   BWHPTYP,BWHPTYPQ    CAMPAIGN/MARKET SEQ NO                       
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,BAGYMD                                                  
         OC    BWHPAGMD,BBYRMASK                                                
         MVC   BWHPBYR,BBYR                                                     
         GOTO1 AIO,DIRHID                                                       
         BNE   VALKX                                                            
         MVC   APHALF,=X'FFFF'                                                  
         CLC   IOKEY(BWHPSEQ-BWHPKEY),IOKEYSAV  TEST FIRST CAMPAIGN             
         BNE   VALK9                            FOR BUYER                       
         SR    RE,RE               NO-NEXT SEQ NO                               
         ICM   RE,3,BWHPSEQ                                                     
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    ECMSEQ                                                           
         STH   RE,APHALF                                                        
*                                                                               
VALK9    MVC   IOKEY,LHDRKEY       SET NEW SEQ NO IN HEADER KEY                 
         MVC   BWHKSEQ,APHALF                                                   
         MVC   LHDRKEY,IOKEY                                                    
         B     VALK15                                                           
*                                                                               
VALK10   TM    APINDS,APIOKCHA     HDR EXISTS - MUST BE ABLE TO CHANGE          
         BZ    VALKX                                                            
         TM    APINDS,APIOKRES                  MUST NOT BE DELETED             
         BO    VALKX                                                            
*                                                                               
VALK11   MVC   LHDRDA,IODA         SAVE THE HEADER DISK ADDRESS                 
*                                                                               
         LA    R1,FILGETU1         GET THE HEADER RECORD                        
******** LA    R1,FILGET1          (ALWAYS GET HEADER RECORD FOR UPDATE         
******** CLI   APACTN,ACTADD       TO AVOID DEADLY EMBRACES IN LIST             
******** BNE   *+8                 SCREENS)                                     
******** LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   APRECNUM,RECPKG     TEST PACKAGE/ORBIT RECORD MAINT              
         BE    *+12                                                             
         CLI   APRECNUM,RECORB                                                  
         BNE   VALK12                                                           
         MVC   IOKEY,APRECKEY      YES - GET THE PACKAGE/ORBIT RECORD           
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALK30                                                           
*                                                                               
VALK12   L     R2,AIOAREA1         DOES THIS STATION ALREADY EXIST?             
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
         SR    R8,R8                                                            
         SR    RF,RF                                                            
*                                                                               
VALK13   CLI   0(R4),0                                                          
         BE    VALK14                                                           
         CLI   0(R4),BWHELCDQ                                                   
         BNE   VALK13A                                                          
         USING BWHEL,R4                                                         
         IC    R8,BWHSEQ                                                        
         CR    R8,RF                                                            
         BNH   *+6                                                              
         LR    RF,R8                                                            
         CLC   BWHSTA,QSTA                                                      
         BE    VALK15                                                           
*                                                                               
VALK13A  IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALK13                                                           
*                                                                               
VALK14   OI    LINDS,LNEWSTA       NEW STATION                                  
         XC    APELEM,APELEM                                                    
         LA    R4,APELEM           ADD NEW STATION ELEMENT TO HEADER            
         MVI   BWHELCD,BWHELCDQ                                                 
         MVI   BWHELLN,BWHELLNQ                                                 
         LA    R8,1(RF)            R8=STATION CODE                              
         CHI   R8,256                                                           
         BL    *+6                                                              
         DC    H'0'                DEEP TROUBLE                                 
         STC   R8,BWHSEQ                                                        
         MVC   BWHSTA,QSTA                                                      
         GOTO1 AADDELS,BWHRECD                                                  
*                                                                               
VALK15   TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    VALK16                                                           
         TM    LINDS,LNOHDR+LNEWSTA  YES-CHECK HDR AND STATION EXIST            
         BNZ   VALK99                                                           
*                                                                               
         CLC   APRECDA,BWHSTA      STATION LETTERS THE SAME (4 BYTES)?          
         BNE   VALK98                                                           
*                                                                               
         L     RF,ALSM             SET THE KEY FROM SAVED STAORAGE              
         LH    R1,LSMRDSP-LSMD(RF)                                              
         AR    R1,RF                                                            
         MVC   APRECKEY,LSMRKEY-LSMRTAB(R1)                                     
         CLM   R8,1,BWDKELST       TEST STATION CODE HAS CHANGED                
         BE    VALK17                                                           
         B     VALK98              YES-ERROR                                    
*                                                                               
VALK16   XC    APRECKEY,APRECKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    SET UP FIRST PART OF DETAIL KEY              
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ     CAMPAIGN/MARKET SEQ NO                       
         MVI   BWDKELCD,1                                                       
         MVI   BWDKELST,1          STATION CODE                                 
         TM    LINDS,LNOHDR                                                     
         BO    *+8                                                              
         STC   R8,BWDKELST                                                      
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY,BDAYS                                                   
         MVC   BWDKELTM,PTIMES                                                  
*                                                                               
         TM    LINDS,LNOHDR        TEST HEADER EXISTS                           
         BZ    VALK17                                                           
         MVI   LSEQNUM,1           NO-SET DETAIL SEQ NUM TO 1                   
         MVI   BWDKELSQ,1                                                       
         B     VALK30              AND EXIT NOW                                 
*                                                                               
VALK17   MVC   IOKEY,APRECKEY                                                   
         MVI   LSEQNUM,0                                                        
         LA    R1,MINHI2                                                        
         B     VALK18+4                                                         
*                                                                               
VALK18   LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    VALK19                                                           
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    TWAMODE,TWAMLSM     EOF-TEST LIST/SELECT MODE                    
         BO    VALK99              YES-SET RECORD NOT FOUND                     
         B     VALK22                                                           
*                                                                               
VALK19   L     R3,AIOAREA2                                                      
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    VALK20                                                           
         CLC   IOKEY(13),IOKEYSAV  YES-KEY MUST MATCH                           
         BNE   VALK99                                                           
         MVC   LSEQNUM,BWDKELSQ                                                 
         B     VALK30                                                           
*                                                                               
VALK20   CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV  NO-MATCH UP TO TIMES            
         BNE   VALK22                                                           
         CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BE    VALK21              YES-ONLY NEED TO GET HIGHEST SEQ NO          
         CLC   BWDTIMES,BTIMES     NO-MATCH ACTUAL TIMES                        
         BNE   VALK18                                                           
         CLC   BWDDPT,BDPT         MATCH DAYPART                                
         BNE   VALK18                                                           
         CLC   BWDSLN,BSLN         MATCH SPOT LENGTH                            
         BNE   VALK18                                                           
         CLI   LSEQNUM,0           TEST ALREADY HAVE A MATCH                    
         BNE   EDUPREC             YES-USER NEEDS THE 'DUP' SCREEN              
*                                                                               
VALK21   MVC   LSEQNUM,BWDKELSQ    SAVE THE SEQUENCE NUMBER                     
         B     VALK18                                                           
*                                                                               
VALK22   CLI   APACTN,ACTADD       TEST ACTION=ADD                              
         BNE   VALK24                                                           
         ZIC   RE,LSEQNUM                                                       
         LA    RE,1(RE)                                                         
         CHI   RE,255                                                           
         BH    ETMD                                                             
         STC   RE,LSEQNUM                                                       
         LA    R3,APRECKEY                                                      
         MVC   BWDKELSQ,LSEQNUM                                                 
         B     VALK26                                                           
*                                                                               
VALK24   CLI   LSEQNUM,0           ACTION NOT ADD - TEST RECORD FOUND           
         BE    VALK26              NO                                           
         TM    LINDS,LNEWSTA       YES-MAKE SURE STATION IS IN HDR REC          
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R3,APRECKEY                                                      
         MVC   BWDKELSQ,LSEQNUM    MOVE SEQ NUM TO KEY                          
         MVC   IOKEY,APRECKEY      AND READ THE RECORD                          
         GOTO1 AMIN,MINRD2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALK30                                                           
*                                                                               
VALK26   OI    APINDS,APIOKADD     OK TO ADD                                    
         NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL-APIOKRES                    
*                                                                               
VALK30   TM    APINDS,APIOKADD     TEST RECORD NOT FOUND                        
         BZ    VALKX                                                            
         TM    TWAMODE,TWAMLSM     AND IN LIST/SELECT MODE                      
         BO    VALK99              YES-RETURN ERROR CODE                        
         B     VALKX                                                            
*                                                                               
VALK98   MVC   FVMSGNO,=AL2(FVFSKHC)    SELECTED RECORD KEY HAS CHANGED         
         LA    R1,REVSTAH          IN PARTICULAR, STATION                       
         ST    R1,FVADDR                                                        
         B     VALKX                                                            
*&&                                                                             
VALK99   MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
*                                                                               
VALKX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
VALREC   TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BNZ   VALRERR              - YUP, NO NBR FOR CANADA!                   
*                                                                               
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD REQUIRED                 
         BZ    VALR1                                                            
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   VALRX                                                            
*                                                                               
VALR1    XC    LSPWEL,LSPWEL                                                    
         XC    LDMOEL,LDMOEL                                                    
         XC    LBWDELSV,LBWDELSV                                                
*                                                                               
         LA    R3,IOKEY                                                         
         MVC   APRECKEY(20),IOKEY      MAKE A COPY                              
*                                                                               
         NI    LINDS,FF-LADDREC-LCHG                                            
         NI    LINDS2,X'FF'-L2RECDEL                                            
         CLI   0(R3),NBRKTYPQ      CHANGING/ADDING BUY REVISION?                
         BH    VALR4               NO, A BUY RECORD!                            
         CLI   APACTN,ACTADD       ADDING A BUY REVISION RECORD?                
         BNE   VALR2               NO, CHANGING A BUY REVISION RECORD           
***********************************                                             
* NO BUY REVISION RECORD, CREATE A FAKE ONE IN AIOAREA3                         
***********************************                                             
         OI    LINDS,LADDREC+LCHG  GOING TO ADD THIS RECORD IF CHANGED          
*                                                                               
         GOTO1 AIO,DIRHI+IO3+IORDEL                                             
         BE    VALR1A                                                           
         TM    IOERR,IOEDEL        RECORD IS DELETED?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALR1A   CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BNE   VALR1C                                                           
*                                                                               
         GOTO1 AIO,FILGETU3+IORDEL  YES, GET NBR TO AIOARE3                     
         BE    VALR1B                                                           
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALR1B   NI    LINDS,X'FF'-LADDREC   NOT ADDING                                 
         OI    LINDS2,L2RECDEL       THE RECORD WAS PREVIOUSLY DELETED          
*                                                                               
VALR1C   L     RE,AIOAREA3                                                      
****     LA    RF,4000                                                          
         LHI   RF,6000                                                          
         XCEFL                                                                  
*                                                                               
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,BSTA                                                     
         MVC   NBRKNBSQ,BSEQNUM                                                 
         MVC   NBRLEN,=Y(NBRSLNQ+NBRFSTEL-NBRKEY)                               
         LA    R2,NBRFSTEL                                                      
         USING NBRSELD,R2                                                       
         MVI   NBRSEL,NBRSELQ                                                   
         MVI   NBRSLN,NBRSLNQ                                                   
         XC    APWORK,APWORK                                                    
         MVC   APWORK+2(L'BSTA),BSTA                                            
         GOTO1 VMSUNPK,APPARM,(X'80',APWORK),APWORK+5,NBRSSTA                   
*                                                                               
         CLI   NBRSSTA+4,C' '                                                   
         BNE   *+16                                                             
         CLI   QMED,C'T'                                                        
         BNE   *+8                                                              
         MVI   NBRSSTA+4,C'T'                                                   
*                                                                               
         B     VALR5                                                            
**********************************                                              
* GET THE BUY REVISION RECORD                                                   
**********************************                                              
VALR2    GOTO1 AIO,DIRHI+IO2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
         B     VALRX                                                            
*                                                                               
         GOTO1 AIO,FILGETU3                                                     
         BE    VALR5                                                            
         DC    H'0'                                                             
**********************************                                              
* GET THE BUY RECORD                                                            
**********************************                                              
VALR4    GOTO1 AIO,DIRHI+IO2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'BUYKEY),IOKEYSAV                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)    RECORD NOT FOUND                        
         B     VALRX                                                            
*                                                                               
         GOTO1 AIO,FILGET2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         USING BUYKEY,R2                                                        
*                                                                               
         USING NBRKEY,R3           NOW SEE IF WE HAVE A BUY REVISION            
         XC    IOKEY,IOKEY             FOR THIS BUY ALREADY                     
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,BSTA                                                     
         MVC   NBRKKBUY,BUYKBUY-BUYKEY+APRECKEY                                 
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,DIRHI+IORDEL+IO3                                             
         BE    VALR4A                                                           
         TM    IOERR,IOEDEL        RECORD IS DELETED?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALR4A   CLC   IOKEY(NBRKNBSQ-NBRKEY),IOKEYSAV                                  
         BE    *+12                FOUND OUR REVISION RECORD                    
         OI    LINDS,LADDREC       GOING TO ADD THIS RECORD IF CHANGED          
         B     VALR4Z                                                           
*                                                                               
         GOTO1 AIO,FILGETU3+IORDEL  YES, GET NBR TO AIOARE3                     
         BE    VALR5                                                            
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OI    LINDS2,L2RECDEL     RECORD WAS DELETED                           
*                                                                               
VALR4Z   GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
         TM    LINDS,LNOTPOCM         NOT PART OF THIS CAMPAIGN?                
         BNZ   VALRX                  NOTHING TO VALIDATE THEN                  
***********************************                                             
* START VALIDATION PROCESS                                                      
***********************************                                             
VALR5    DS    0H                                                               
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
*                                                                               
         BRAS  RE,CPYDMOSP                                                      
*                                                                               
         LA    R2,NBRFSTEL                                                      
         USING NBRSELD,R2                                                       
*                                                                               
         GOTO1 AFVAL,REVDAYH                                                    
         BH    EIIF                                                             
         BL    ENOC                                                             
         BAS   RE,CKVLDATD         CHECK IF VALIDATED PREVIOUSLY                
         B     VALR10                                                           
*                                                                               
CKVLDATD TM    FVIHDR+4,X'20'      PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    LINDS,LCHG          NO, CHANGED FIELD                            
         BR    RE                                                               
*                                                                               
VALR10   GOTO1 AVALDAY,REVDAYH                                                  
         CLI   BDAYS,0                                                          
         BE    EIIF                                                             
         TM    LINDS,LCHG          NEW DAYS?                                    
         BZ    *+8                                                              
         OI    LINDS2,L2DTUCHG      - YUP, MAY NEED NEW PROGRAM NAME            
         MVC   NBRSDAYS,BDAYS                                                   
*                                                                               
         GOTO1 AFVAL,REVTIMH                                                    
         BH    EIIF                                                             
         BL    ENOC                                                             
         BAS   RE,CKVLDATD                                                      
         GOTO1 AVALTIM,REVTIMH                                                  
         BNE   VALRX                                                            
         TM    LINDS,LCHG          NEW DAYS?                                    
         BZ    *+8                                                              
         OI    LINDS2,L2DTUCHG      - YUP, MAY NEED NEW PROGRAM NAME            
         MVC   NBRSTIMS(L'BTIMES),BTIMES                                        
*                                                                               
         GOTO1 AVALDPL,REVDPLH                                                  
         BNE   VALRX                                                            
*                                                                               
         CLI   BDPT,0                                                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVIDPT)                                             
         B     VALRX                                                            
         MVC   NBRSDYPT,BDPT                                                    
*                                                                               
         CLI   BSLN,0                                                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVISLN)                                             
         B     VALRX                                                            
         MVC   NBRSSLN,BSLN                                                     
*                                                                               
         CLI   CMPDPOPT,C'M'       TEST SUBDPTS SCHEDULED UNDER MASTER          
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                    YES-ERROR                                
*                                                                               
         BAS   RE,CKVLDATD                                                      
*                                                                               
         MVI   LSUBDPT,0           ONLY SUBDAYPART IN THIS FIELD                
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,REVSDPH                                                    
         BH    VALRX                                                            
         BL    VALR12              NONE, WE ALREADY INITIALIZED TO 0            
         CLI   CMPDPOPT,C'S'       TEST SCHEDULE SUBDPTS SEPERATELY             
         BE    EIIF                YES-INVALID INPUT FIELD                      
*                                                                               
         BAS   RE,CKVLDATD                                                      
         MVC   LSUBDPT,FVIFLD                                                   
*                                                                               
         LA    RE,L'DPTSUBS                                                     
         LA    RF,DPTSUBS                                                       
         CLC   FVIFLD(1),0(RF)                                                  
         BE    VALR12                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
         MVC   FVMSGNO,=AL2(FVIDPT)                                             
         B     VALPX                                                            
*                                                                               
VALR12   MVC   NBRSSBDP,LSUBDPT    SO IT WOULD BE 0 IF NONE                     
*                                                                               
         MVI   APFLAG,0                                                         
         XC    APDUB,APDUB                                                      
         LA    R1,REVCS1H          COST                                         
         MVI   FVILEN-FVIHDR(R1),3                                              
         CLI   FVILEN-FVIHDR(R1),0                                              
         BNE   VALR12E                                                          
         OC    REVCS1,REVCS1                                                    
         BZ    ENOC                                                             
*                                                                               
VALR12E  LA    R8,NBRSCST1                                                      
         BAS   RE,VALRCST                                                       
         BAS   RE,CKVLDATD                                                      
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         BAS   RE,DISRCST                                                       
*                                                                               
         LA    R1,REVEF2H          EFFECTIVE DATE 2                             
         LA    R8,NBRSEDT2                                                      
         BAS   RE,VALRDAT                                                       
         BAS   RE,CKVLDATD                                                      
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    VALR14                                                           
         CLC   NBRSEDT2,CMPST                                                   
         BNH   EDLO                                                             
         CLC   NBRSEDT2,CMPND                                                   
         BH    EDHI                                                             
         GOTO1 AVALRDAY                                                         
         BNE   EDIV                                                             
         CLC   CMPSTMON,APWORK     TEST EFFECTIVE DATE IN SAME WEEK             
         BE    EDIV                AS START DATE                                
         MVC   APWORK+12(6),APWORK                                              
*                                                                               
VALR14   LA    R1,REVCS2H          COST 2                                       
         LA    R8,NBRSCST2                                                      
         BAS   RE,VALRCST                                                       
         BAS   RE,CKVLDATD                                                      
         OC    NBRSCST2,NBRSCST2                                                
         BZ    *+8                                                              
         BAS   RE,DISRCST                                                       
*                                                                               
         LA    R1,REVEF3H          EFFECTIVE DATE 3                             
         LA    R8,NBRSEDT3                                                      
         BAS   RE,VALRDAT                                                       
         BAS   RE,CKVLDATD                                                      
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    VALR16                                                           
         CLC   NBRSEDT3,NBRSEDT2                                                
         BNH   EDLL                                                             
         CLC   NBRSEDT3,CMPND                                                   
         BH    EDHI                                                             
         GOTO1 AVALRDAY                                                         
         BNE   EDIV                                                             
         CLC   APWORK+12(6),APWORK TEST EFFECTIVE DATE 3 IN SAME WEEK           
         BE    EDIV                AS EFFECTIVE DATE 2                          
*                                                                               
VALR16   LA    R1,REVCS3H          COST 3                                       
         LA    R8,NBRSCST3                                                      
         BAS   RE,VALRCST                                                       
         BAS   RE,CKVLDATD                                                      
         OC    NBRSCST3,NBRSCST3                                                
         BZ    *+8                                                              
         BAS   RE,DISRCST                                                       
*                                                                               
         NI    LINDS2,X'FF'-L2PRGCHG                                            
*                                                                               
****  CODE FOR THE NEW REP FIELD ON THE SCREEN                                  
         OC    REVREPN,REVREPN     ANYTHING HERE? (REVREPNH+5NO GOOD)           
***      BZ    VALR16G              - NOPE, NOTHING                             
         BNZ   VALR16E             NON-ZERO                                     
         XC    NBRSREP,NBRSREP     TAKE OUT THE REP                             
         XC    LESTREP,LESTREP     DELIBERATELY NO REP                          
         OI    LINDS,LCHG          CHANGED FIELD                                
         B     VALR16S                                                          
*                                                                               
VALR16E  BRAS  RE,REPCHK                                                        
         BNE   EBADREP                                                          
*                                                                               
*        MVC   NBRSREP,REVREPN                                                  
         MVC   NBRSREP,LREP        NOW A BINARY FIELD!!                         
         OI    LINDS,LCHG          CHANGED FIELD                                
         B     VALR16S                                                          
*                                                                               
VALR16G  OC    ESTREP,ESTREP       IS THERE AN ESTIMATE REP?                    
         BZ    VALR16S              - NOPE                                      
         MVC   NBRSREP,ESTREP      BINARY                                       
         MVC   REVREPN,LESTREP     EBCDIC                                       
         MVI   REVREPNH+5,3                                                     
         OI    REVREPNH+6,X'80'                                                 
         OI    LINDS,LCHG          CHANGED FIELD                                
****                                                                            
VALR16S  GOTO1 ACHKBPPR,REVPRGH                                                 
         BNE   VALRX                                                            
*                                                                               
         GOTO1 AFVAL,REVPRGH       PROGRAMMING                                  
         BH    VALRX                                                            
         BL    VALR18                                                           
         BAS   RE,CKVLDATD                                                      
         CLI   FVIFLD,C'$'         TEST PROGRAM ADJACENCY CODE                  
         BNE   VALR17                                                           
         GOTO1 AVALADJ,FVIFLD+1    YES - VALIDATE IT                            
         BNE   VALRX                                                            
         BAS   RE,CKVLDATD                                                      
         MVC   NBRSADJC,QADJCD     VALID - SET THE ADJACENCY CODE               
         B     VALR23                                                           
*                                                                               
VALR17   CLI   APACTN,ACTADD       PROGRAM NOT BLANK-                           
         BE    VALR20              IF ADD, THEN IT'S AN OVERRIDE                
         B     VALR19              ELSE, TEST FOR PROGRAM CHANGE                
*                                                                               
VALR18   CLI   APACTN,ACTADD       PROGRAM BLANK-                               
         BE    VALR22              IF ADD, LEAVE PROGRAM BLANK FOR NOW          
*                                                                               
VALR19   CLC   NBRSPROG,FVIFLD     TEST PROGRAM CHANGE                          
         BE    VALR22                                                           
VALR20   OI    LINDS2,L2PRGCHG     PROGRAMMING OVERRIDE                         
         OI    NBRSINDS,NBRSIPRG   PROGRAMMING OVERRIDE BIT                     
VALR22   MVC   NBRSPROG,FVIFLD                                                  
*                                                                               
VALR23   TM    LINDS2,L2COS2       WE NEED COS2?                                
         BZ    VALR24               - NOPE                                      
*                                                                               
         MVI   APELEM,NBRC2ELQ                                                  
         GOTO1 ADELELS,NBRRECD                                                  
*                                                                               
         XC    APELEM,APELEM       ADD COS2 ELEMENT                             
         LA    R4,APELEM                                                        
         USING NBRC2EL,R4                                                       
         LA    R1,REVC2NH                                                       
         LA    R8,NBRC2CST                                                      
         BAS   RE,VALRCST                                                       
         BAS   RE,CKVLDATD         CHECK IF VALIDATED PREVIOUSLY                
         OC    NBRC2CST,NBRC2CST   DID WE HAVE ANYTHING?                        
         BZ    VALR24                                                           
         BAS   RE,DISRCST                                                       
         MVI   NBRC2EL,NBRC2ELQ    X'32' ELEMENT                                
         MVI   NBRC2LEN,NBRC2LNQ                                                
***      MVC   NBRC2CST,APWORK     SAVE IT!                                     
         GOTO1 AADDELS,NBRRECD                                                  
         DROP  R4                                                               
*                                                                               
VALR24   GOTO1 AFVAL,REVIDH        VALIDATE ID                                  
         BNE   VALR25                                                           
         BAS   RE,CKVLDATD                                                      
         XC    APELEM,APELEM       ADD ID ELEMENT                               
         MVI   APELEM,NBRIDELQ                                                  
         GOTO1 ADELELS,NBRRECD     DELETE ALL ID ELEMENTS                       
*                                                                               
         LA    R4,APELEM                                                        
         USING NBRIDELD,R4                                                      
         MVI   NBRIDEL,NBRIDELQ                                                 
         MVI   NBRIDLEN,NBRIDLNQ                                                
         MVC   NBRIDID,FVIFLD                                                   
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
VALR25   GOTO1 AFVAL,REVDATH       VALIDATE DATES                               
         BE    *+14                                                             
         XC    NBRSDTES,NBRSDTES                                                
         B     VALR25A                                                          
*                                                                               
         GOTO1 AVALDATE,REVDATH    VALIDATE DATES                               
         BNE   VALRX                                                            
         MVC   NBRSDTES,APFULL     SET DATES                                    
         MVC   NBRSWKS,APHALF      SET INACTIVE WEEKS                           
         MVC   NBRSWKS2,APDUB      GET OTHER INACTIVE WEEKS                     
         CLI   APACTN,ACTADD       IS IT AN ADD?                                
         BE    VALR25A              - YUP, SKIP FOLLOWING SUBROUTINE            
         BRAS  RE,INAWKCHK         CHK EXISTING SPOTS FOR INACTIVE WKS          
         BNE   EINAWK               - INACTIVE WEEKS HAVE SPOTS, ERROR          
*                                                                               
VALR25A  TM    FVIHDR+4,X'20'      PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    LINDS,LCHG          NO, CONSIDERED CHANGED                       
*&&DO                                                                           
VALR25A  CLI   LSPWEL,0            TEST FOR SPOTS PER WEEK ELEMENT              
         BE    VALR26                                                           
         MVC   APELEM(L'LSPWEL),LSPWEL                                          
         GOTO1 AADDELS,NBRRECD     YES - ADD IT                                 
         B     VALR29                                                           
*                                                                               
VALR26   LA    R4,LBTREL           ADD BACK IN THE BUY TRANFER ELES             
         BAS   RE,VALR28                                                        
         LA    R4,LBTREL2                                                       
         BAS   RE,VALR28                                                        
         LA    R4,LBTREL3                                                       
         BAS   RE,VALR28                                                        
         LA    R4,LDTREL                                                        
         BAS   RE,VALR28                                                        
         B     VALR29                                                           
*                                                                               
VALR28   LR    R0,RE                                                            
VALR28A  CLI   0(R4),0                                                          
         BE    VALR28X                                                          
         XC    APELEM,APELEM                                                    
         ZIC   R8,1(R4)                                                         
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R4)                                                  
         GOTO1 AADDELS,NBRRECD                                                  
         LA    R4,1(R8,R4)                                                      
         B     VALR28A                                                          
VALR28X  LR    RE,R0                                                            
         BR    RE                                                               
*&&                                                                             
VALR29   LA    R4,APELEM                                                        
         XC    LUPFIL,LUPFIL       CLEAR UPGRADE VALUES                         
         XC    LUPGRD,LUPGRD                                                    
         XC    LUPFRBK,LUPFRBK                                                  
         MVI   LUPFRBKT,0                                                       
         XC    LUPFRBKL,LUPFRBKL                                                
         XC    LUPPUT,LUPPUT                                                    
         XC    LUPSHR,LUPSHR                                                    
         XC    LOVDAY,LOVDAY                                                    
         XC    LOVTIME,LOVTIME                                                  
*                                                                               
         TM    REVUPGH+4,X'20'     PREVIOUSLY VALIDATED?                        
         BNZ   VALR29G                                                          
         OI    LINDS,LCHG          NO, A CHANGE WAS MADE                        
         OI    LINDS2,L2DTUCHG      - MAY NEED PROGRAM NAME LOOKUP              
VALR29G  MVI   APELEM,NBRUPELQ                                                  
         GOTO1 ADELELS,NBRRECD                                                  
         CLI   REVUPGH+5,0         IF NO UPGRADE INPUT                          
         BNE   VALR30              AND THERE IS A DEFAULT                       
*                                                                               
         OC    CMPUP,CMPUP         CAMPAIGN UPGRADE, THEN SAVE                  
         BZ    VALR31              THE DEFAULT VALUES                           
         MVC   LUPFIL,CMPUF                                                     
         MVC   LUPGRD,CMPUP                                                     
         MVC   LUPFRBK,CMPFB                                                    
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   VALR29R                                                          
         CLI   CMPFBTP,0           ANYTHING HERE?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LUPFRBKT,CMPFBTP                                                 
VALR29R  MVC   LUPFRBKL,CMPFBLST                                                
         MVC   LUPPUT,CMPUPUT                                                   
         MVC   LUPSHR,CMPUSHR                                                   
         MVI   QBOOKTYP,0          MUST BE CLEARED BECAUSE DEFAULT              
         TM    LUPFRBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    VALR31                                                           
         TM    LUPFRBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   VALR29T              - NOPE                                      
         CLI   LUPFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QBOOKTYP,LUPFRBKT    - YUP                                       
         B     VALR31                                                           
*                                                                               
VALR29T  GOTO1 AGETBKTY,APPARM,(C'B',LUPFRBK+1),QBOOKTYP                        
         B     VALR31                                                           
*                                                                               
VALR30   MVI   APFLAG,X'F8'        UPGRADE EXPRESSION                           
         GOTO1 AVALUPG,REVUPGH                                                  
         BNE   VALRX                                                            
*                                                                               
         MVC   NBRSUPUT,APWORK+16                                               
         MVC   NBRSUSHR,APWORK+17                                               
*                                                                               
         LA    R4,APELEM                                                        
         XC    APELEM,APELEM       BUILD UPGRADE ELEMENT                        
         USING NBRUPELD,R4                                                      
         MVI   NBRUPEL,NBRUPELQ                                                 
         MVI   NBRUPLEN,NBRUPLNQ                                                
         MVC   NBRUPFIL,APWORK                                                  
         MVC   NBRUPEXP,APWORK+1                                                
         MVC   NBRUPOBK,APWORK+9                                                
         TM    APWORK+9+1,BTY2CHAR   2 CHARACTER BOKOTYPE?                      
         BNO   VALR30E                                                          
         CLI   QBOOKTYP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   NBRUPOBT,QBOOKTYP                                                
         MVI   NBRUPLEN,NBRUPLQ2   EXTENDED LENGTH                              
VALR30E  MVC   NBRUPBKL,APWORK+18                                               
         MVC   NBRUPINP,SPACES                                                  
         ZIC   RE,REVUPGH+FVILEN-FVIHDR                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NBRUPINP(0),REVUPG                                               
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
         MVC   LUPFIL,NBRUPFIL     SAVE UPGRADE VALUES                          
         MVC   LUPGRD,NBRUPEXP                                                  
         MVC   LUPFRBK,NBRUPOBK                                                 
         TM    NBRUPOBK+1,BTY2CHAR   2 CHARACTER BOOKTYPE + L?                  
         BNO   VALR30G                                                          
         CLI   NBRUPLEN,NBRUPLQ2   EXTENDED LENGTH?                             
         BE    *+6                  - YUP YUP                                   
         DC    H'0'                                                             
         MVC   LUPFRBKT,NBRUPOBT                                                
VALR30G  MVC   LUPFRBKL,NBRUPBKL                                                
         MVC   LUPPUT,NBRSUPUT                                                  
         MVC   LUPSHR,NBRSUSHR                                                  
*                                                                               
         OC    APWORK+11(5),APWORK+11                                           
         BZ    VALR31                                                           
         LA    R4,APELEM                                                        
         USING NBRODELD,R4                                                      
         XC    APELEM,APELEM       BUILD OVERRIDE DAY/TIME/STATION ELE          
         MVI   NBRODEL,NBRODELQ                                                 
         MVI   NBRODLEN,NBRODLNQ                                                
         MVC   NBRODODY,APWORK+11                                               
         MVC   NBRODOTM,APWORK+12                                               
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
         MVC   LOVDAY,NBRODODY     SAVE OVERRIDE DAY/TIME                       
         MVC   LOVTIME,NBRODOTM                                                 
         DROP  R4                                                               
*&&DO                                                                           
VALR31   MVC   NBRUCODE,SPACES     USER CODE                                    
         GOTO1 AFVAL,REVCODH                                                    
         BH    VALRX                                                            
         BL    *+16                                                             
         MVC   NBRUCODE,REVCOD                                                  
         OC    NBRUCODE,SPACES                                                  
         BAS   RE,CKVLDATD                                                      
*&&                                                                             
VALR31   DS    0H                                                               
         CLI   APACTN,ACTADD       ADDING WITH                                  
         BNE   VALR31Y                                                          
         CLC   =C'SKED=',REVCM1    SCHEDULE OPTION                              
         BNE   VALR31Y                                                          
         GOTO1 AFVAL,REVCM1H                                                    
         BAS   RE,CKVLDATD                                                      
*                                                                               
         GOTO1 VSCANNER,APPARM,(X'80',REVCM1H),(15,LIUN)                        
         CLI   APPARM+4,0                                                       
         BE    EIIF                                                             
*                                                                               
         MVI   APELEM,NBRSPELQ                                                  
         GOTO1 ADELELS,NBRRECD                                                  
*                                                                               
         LA    R4,APELEM                                                        
         USING NBRSPELD,R4                                                      
         XC    APELEM,APELEM                                                    
         MVI   NBRSPEL,NBRSPELQ                                                 
*                                                                               
         LA    R1,LIUN             R1 = 1ST SCAN ENTRY                          
         ZIC   RF,APPARM+4         RF = # OF SCAN RECORDS                       
         LA    RE,NBRSPSPW         RE = 1ST SPOT WEEK                           
*                                                                               
         CLI   1(R1),0             NOTHING FOR THE SKED=                        
         BE    EIIF                                                             
         TM    3(R1),X'80'         VALID NUMERIC?                               
         BZ    EIIF                                                             
         OC    9(2,R1),9(R1)       MAKE SURE NOT MORE THAN 255                  
         BNZ   EIIF                                                             
         MVC   0(1,RE),11(R1)      COPY THE NUMBER OF SPOTS                     
         B     VALR31B                                                          
*                                                                               
VALR31A  CLI   0(R1),0             THIS WEEK HAS NO SPOTS?                      
         BE    VALR31B                                                          
         TM    2(R1),X'80'         VALID NUMERIC?                               
         BZ    EIIF                                                             
         OC    5(2,R1),5(R1)       MAKE SURE NOT MORE THAN 255                  
         BNZ   EIIF                                                             
         MVC   0(1,RE),7(R1)       COPY THE NUMBER OF SPOTS                     
*                                                                               
VALR31B  LA    R1,32(R1)                                                        
         LA    RE,1(RE)                                                         
         BCT   RF,VALR31A                                                       
*                                                                               
         CLI   0(RE),0             FIND LAST WEEK WITH SPOTS                    
         BNE   *+8                 SO WE CAN CALCULATE L(ELEMENT)               
         BCT   RE,*-8                                                           
         LR    R0,RE                                                            
         SR    R0,R4                                                            
         CHI   R0,NBRSPSPW-NBRSPELD                                             
         BNH   VALR31X                                                          
         AHI   R0,1                                                             
         STC   R0,NBRSPLEN                                                      
*                                                                               
         GOTO1 AADDELS,NBRRECD     ADD THE ELEMENT TO RECORD                    
*                                                                               
VALR31X  LA    R0,4                                                             
         LA    R9,REVCM2H                                                       
         B     VALR31Z                                                          
***********************************                                             
* VALIDATE THE COMMENT LINES                                                    
***********************************                                             
VALR31Y  LA    R0,5                MAX 5                                        
         LA    R4,APELEM                                                        
         LA    R9,REVCM1H          COMMENT ELEMENTS                             
VALR31Z  SR    R8,R8                                                            
         MVI   APELEM,NBRCMELQ                                                  
         GOTO1 ADELELS,NBRRECD     DELETE ALL COMMENT ELEMENTS                  
*                                                                               
VALR32   LR    R1,R9                                                            
         TM    4(R9),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    LINDS,LCHG                                                       
*                                                                               
         GOTO1 AFVAL                                                            
         BH    VALRX                                                            
         BL    VALR33                                                           
         XC    APELEM,APELEM                                                    
         USING NBRCMELD,R4                                                      
         MVI   NBRCMEL,NBRCMELQ                                                 
         LNR   RF,R0                                                            
         LA    RF,6(RF)                                                         
         STC   RF,NBRCMLIN         COMMENT LINE NUMBER                          
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   NBRCMCOM(0),FVIFLD                                               
         AHI   RE,NBRCMOVQ+1                                                    
         STC   RE,NBRCMLEN                                                      
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
VALR33   AHI   R9,REVCM2-REVCM1        NEXT COMMENT                             
         BCT   R0,VALR32                                                        
***********************************************************************         
* VALIDATE THE HIDDEN FIELDS REVSK1 AND REVSK2                                  
***********************************************************************         
         CLI   APACTN,ACTADD       ADDING WITH                                  
         BNE   VALR33X                                                          
         CLI   REVSK1H+5,0         SCHEDULE OPTION AND WORK ">"?                
         BE    VALR33X             NO INPUT                                     
         GOTO1 AFVAL,REVSK1H                                                    
         BAS   RE,CKVLDATD                                                      
*                                  NOT USING SCANNER, SAVES ROOM                
         MVI   APELEM,NBRSPELQ                                                  
         GOTO1 ADELELS,NBRRECD                                                  
*                                                                               
         LA    R4,APELEM                                                        
         USING NBRSPELD,R4                                                      
         XC    APELEM,APELEM                                                    
         MVI   NBRSPEL,NBRSPELQ                                                 
*                                                                               
         LA    RE,NBRSPSPW         RE = 1ST SPOT WEEK                           
         ZIC   RF,REVSK1H+5                                                     
         SRL   RF,1                2 DIGITS PER WEEK                            
         LA    R1,REVSK1           R1 = 1ST WEEK                                
*                                                                               
VALR33A  PACK  APDUB,0(2,R1)                                                    
         CVB   R0,APDUB                                                         
         STC   R0,0(RE)                                                         
*                                                                               
         LA    RE,1(RE)                                                         
         LA    R1,2(R1)                                                         
         BCT   RF,VALR33A          THIS SHOULD PROTECT ME                       
         LA    R0,REVSK2                                                        
         CR    R1,R0               PROCESSED 2ND LINE ALREADY?                  
         BNL   VALR33H             YES, READY TO ADD THE ELEMENT                
         CLI   REVSK2H+5,0         NO, ANYTHING ON THE 2ND LINE?                
         BE    VALR33H                 NOTHING LEFT TO PROCESS                  
         ZIC   RF,REVSK2H+5                                                     
         SRL   RF,1                2 DIGITS PER WEEK                            
         LA    R1,REVSK2           R1 = 1ST WEEK OF 2ND LINE                    
         B     VALR33A                                                          
*                                                                               
VALR33H  ZIC   R0,CMPNWKS                                                       
         AHI   R0,NBRSPSPW-NBRSPELD                                             
         STC   R0,NBRSPLEN                                                      
*                                                                               
         GOTO1 AADDELS,NBRRECD     ADD THE ELEMENT TO RECORD                    
*                                                                               
VALR33X  DS    0H                                                               
***********************************                                             
* VALIDATE THE DEMOS                                                            
***********************************                                             
VALR34   MVI   APELEM,NBRDMELQ                                                  
         GOTO1 ADELELS,NBRRECD                                                  
*                                                                               
         LA    R4,APELEM                                                        
         USING NBRDMELD,R4                                                      
         XC    APELEM,APELEM       DEMOS                                        
         MVI   NBRDMEL,NBRDMELQ                                                 
         LA    R0,LNDEMOS                                                       
         LA    R2,ESTDEMS                                                       
         LA    R6,LDEMOVR                                                       
         LA    R8,REVDM1H                                                       
         LA    R9,NBRDMDMO                                                      
         MVI   0(R6),0                                                          
*                                                                               
VALR36   MVC   0(3,R9),0(R2)       DEMO CODE                                    
         LR    R1,R8                                                            
         GOTO1 AFVAL               VALIDATE DEMO VALUE FIELD                    
         BL    VALR42              MISSING - NOT OVERRIDE                       
         BH    VALRX                                                            
         BAS   RE,CKVLDATD                                                      
         XC    LTRT,LTRT           LOOK FOR * IN FIELD                          
         LA    R1,C'*'                                                          
         LA    R1,LTRT(R1)                                                      
         MVI   0(R1),C' '                                                       
         ST    R2,APFULL                                                        
         ZIC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         TRT   FVIFLD(0),LTRT                                                   
         L     R2,APFULL                                                        
         BZ    VALR38               NO *                                        
         MVI   0(R1),C' '           YES - REPLACE WITH SPACE                    
         B     VALR40                                                           
*                                                                               
VALR38   TM    FVIIND,FVIVAL       IF NOT PREV VALIDATED, MUST BE               
         BZ    VALR40              AN OVERRIDE                                  
         CLI   APACTN,ACTADD       TEST ACTION IS ADD                           
         BNE   VALR42              NO - NOT OVERRIDE                            
*                                                                               
VALR40   ZIC   RF,FVILEN           DEMO IS AN OVERRIDE                          
         LA    RE,FVIFLD                                                        
         ST    RE,APPARM           A(INPUT FIELD)                               
         MVI   APPARM,1            DECIMAL POINTS                               
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALR41C                                                          
         CLI   1(R9),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R9),C'E'                                                       
         BNE   VALR41C             IT IS IMPRESSION                             
         MVI   APPARM,2            SHOULD BE 2 DECIMALS                         
***  2 DECIMAL  ***                                                             
VALR41C  ST    RF,APPARM+4         LENGTH                                       
         GOTO1 VCASHVAL,APPARM     VALIDATE DEMO VALUE                          
         CLI   APPARM,X'FF'                                                     
         BE    EIIF                                                             
         MVC   4(4,R9),APPARM+4    DEMO VALUE                                   
         OI    4(R9),NBRDMOOV      INDICATE OVERRIDE                            
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   IS IT 2 DECIMALS?                            
         BNO   VALR41E              - NOPE, NO NEED FOR FLAG                    
         CLI   1(R9),C'R'                                                       
         BE    *+12                                                             
         CLI   1(R9),C'E'                                                       
         BNE   VALR41E             IT IS IMPRESSION                             
         OI    4(R9),NBRDMO2D      INDICATE 2 DECIMAL PRECISION                 
***  2 DECIMAL  ***                                                             
VALR41E  MVI   0(R6),OVERELEM     BUILD SPDEMUP DEMO OVERRIDE LIST ELEM         
         MVI   1(R6),6                                                          
         MVC   2(2,R6),1(R2)                                                    
         MVC   4(2,R6),APPARM+6                                                 
         LA    R6,6(R6)                                                         
         MVI   0(R6),0                                                          
*                                                                               
VALR42   LA    R9,L'NBRDMDMO(R9)   NEXT DEMO                                    
         LA    R2,3(R2)                                                         
         OC    0(3,R2),0(R2)       TEST FOR ANY MORE DEMOS                      
         BZ    *+8                                                              
         BCT   R0,*+12                                                          
         MVI   0(R9),X'FF'         NO - INDICATE END IN THE DEMO ELEM           
         B     VALR44                                                           
         SR    R1,R1               FIND NEXT DEMO VALUE FIELD                   
         IC    R1,0(R8)                                                         
         AR    R8,R1                                                            
         TM    FVATRB-FVIHDR(R8),FVAPROT                                        
         BO    *-10                                                             
         B     VALR36                                                           
*                                                                               
VALR44   SR    R9,R4              DETERMINE DEMO ELEMENT LENGTH                 
         CH    R9,=H'2'                                                         
         BNH   VALR80                                                           
         STC   R9,NBRDMLEN                                                      
         DROP  R4                                                               
*                                                                               
         OC    LUPGRD,LUPGRD       TEST UPGRADE EXPRESSION EXISTS               
         BNZ   VALR44A                                                          
         CLI   QMED,C'R'                                                        
         BNE   ENUE                                                             
         B     VALR64K                                                          
*                                                                               
VALR44A  LA    R6,LDMUPBLK         BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R6                                                      
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
         BL    VALR44E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),QSTA+5   MOVE THE NETWORK IN                          
         MVC   SPUPSYSE,QSTA                                                    
         B     VALR44G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
VALR44E  MVC   SPUPSTA,QSTA                                                     
VALR44G  MVC   SPUPDAY,BDAYS                                                    
         MVC   SPUPTIM,BTIMES                                                   
         MVC   SPUPFIL,LUPFIL                                                   
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,LUPFRBK                                                  
         MVC   SPUPFBKL,LUPFRBKL                                                
         OC    SPUPFBK,SPUPFBK                                                  
         BNZ   *+16                                                             
         MVC   SPUPFBK,ESTBOOK                                                  
         XC    SPUPFBKL,SPUPFBKL                                                
         MVC   SPUPUDAY,LOVDAY                                                  
         MVC   SPUPUTIM,LOVTIME                                                 
         MVC   SPUPTYPE(L'LUPGRD),LUPGRD                                        
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
         TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    VALR44T                                                          
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   VALR44Q              - NOPE                                      
         CLI   LUPFRBKT,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SPUPBTYP,LUPFRBKT    - YUP                                       
         B     VALR44R                                                          
*                                                                               
VALR44Q  GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
VALR44R  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
VALR44T  CLI   CUDMED,C'C'         TEST CANADA                                  
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
         CLI   LUPPUT,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   LUPPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   LUPSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   LUPSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
*                                                                               
         NI    LINDS,FF-LAUTADJ-LALLADJ                                         
         TM    INFIND,INFINOAD     TEST OPTION TO NOT AUTO ADJUST DEMOS         
         BO    VALR60                                                           
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT  NO-TEST FOR AUTOADJ          
         BZ    VALR60                                                           
         OI    LINDS,LAUTADJ       YES                                          
         XC    LADJDEM,LADJDEM                                                  
         TM    CMPOPTS,CAMOAALL+CAMOATGT   TEST ALL OR TGT                      
         BZ    VALR45                                                           
         MVC   LADJDEM,=X'D901'    YES-SET ADJUSTMENT DEMO                      
         TM    CMPOPTS,CAMOAALL                                                 
         BO    VALR45                                                           
         MVC   LADJDEM,ESTDEMS+1                                                
*                                                                               
VALR45   LA    R9,LIMPVALS                                                      
         MVI   0(R9),FF            INIT IMPRESSION ADJUSTS                      
         MVI   LALLVALS,FF         INIT ALL/TGT ADJUSTS                         
         MVI   LDEMS,0                                                          
         MVI   LDEMS+3,FF                                                       
         LA    R2,LDEMOVR                                                       
*                                                                               
VALR46   CLI   0(R2),0             SCAN OVERRIDE ELEM FOR OVERRIDES             
         BE    VALR60                                                           
         OC    LADJDEM,LADJDEM     TEST FOR ADJUSTMENT DEMO                     
         BZ    VALR52                                                           
         CLC   LADJDEM,2(R2)       YES-IS THIS IT?                              
         BNE   VALR52                                                           
         OI    LINDS,LALLADJ       YES-INDICATE TO ADJUST ALL                   
         MVC   LDEMS+1(2),2(R2)    GET RATING                                   
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALR46G                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
VALR46G  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS                          
         XC    LDEMNEW,LDEMNEW                                                  
         MVC   LDEMNEW+2(2),4(R2)                                               
         MVC   LDEMOLD,LDEMVALS                                                 
         GOTO1 AADJCALC            CALCULATE ADJUSTMENT FOR NEW                 
         BNE   VALRX                                                            
         MVC   LADJUST,LDEMADJ     DEMOS                                        
         BAS   RE,GETOLD           FIND RTG IN OLD DEMO ELEM                    
         BNE   VALR58                                                           
         XC    LDEMOLD,LDEMOLD     FOUND - CALCULATE ADJUSTMENT                 
***      MVC   LDEMOLD+1(3),5(R1)                                               
         MVC   LDEMOLD,4(R1)                                                    
         GOTO1 AADJCALC            (LDEMADJ = ADJUSTMENT PERCENTAGE)            
         BNE   VALRX                                                            
         OC    LDMOEL,LDMOEL                                                    
         BZ    VALR58                                                           
         LA    RF,LDMOEL           SCAN OLD DEMO ELEMNT TO BUILD LIST           
         USING NBRDMELD,RF         OF AUTO OVERRIDES                            
         LA    R0,L'NBRDMDMO                                                    
         ZIC   R1,NBRDMLEN                                                      
         AR    R1,RF                                                            
         BCTR  R1,0                                                             
         LA    RF,NBRDMDMO                                                      
         XC    LDEMOLD,LDEMOLD                                                  
         LA    R8,LALLVALS                                                      
*                                                                               
VALR48   CLC   1(2,RF),LADJDEM     TEST ADJUSTMENT DEMO                         
         BE    VALR50              YES - SKIP                                   
         TM    4(RF),NBRDMOOV      TEST ALREADY MANUALLY OVERRIDDEN             
         BO    VALR50              YES - SKIP                                   
***      MVC   LDEMOLD+1(3),5(RF)                                               
         MVC   LDEMOLD,4(RF)                                                    
         BAS   RE,ADJUST           ADJUST                                       
         MVC   0(2,R8),1(RF)                                                    
         MVC   2(4,R8),LDEMNEW     SAVE NEW AUTO-ADJUSTED DEMO VALUE            
         LA    R8,6(R8)                                                         
         MVI   0(R8),FF                                                         
VALR50   BXLE  RF,R0,VALR48                                                     
         B     VALR58                                                           
         DROP  RF                                                               
*                                                                               
VALR52   CLI   2(R2),C'R'          NOT AN 'ALL' ADJUSTMENT - TEST RTG           
         BNE   VALR58                                                           
         MVI   0(R9),C'I'          YES - ENTER IN IMP LIST                      
         MVC   1(1,R9),3(R2)                                                    
         MVC   LDEMS+1(2),0(R9)                                                 
         BAS   RE,GETOLD           FIND IMPRESSION IN OLD DEMO ELEM             
         BNE   VALR54                                                           
         TM    4(R1),NBRDMOOV      FOUND - TEST MANUALLY OVERRIDDEN             
         BO    VALR54                                                           
         ST    R1,APDUB            NO - APDUB = A(IMP ENTRY)                    
         MVI   LDEMS+1,C'R'             FIND RATING IN OLD DEMO ELEM            
         BAS   RE,GETOLD                                                        
         BNE   VALR54                                                           
         XC    LDEMOLD,LDEMOLD     FOUND - CALCULATE PCT ADJUST                 
***      MVC   LDEMOLD+1(3),5(R1)                                               
         MVC   LDEMOLD,4(R1)                                                    
         XC    LDEMNEW,LDEMNEW                                                  
         MVC   LDEMNEW+2(2),4(R2)                                               
         GOTO1 AADJCALC                                                         
         BNE   VALRX                                                            
         L     R1,APDUB            AUTO ADJUST THE IMPRESSION                   
***      MVC   LDEMOLD+1(3),5(R1)                                               
         MVC   LDEMOLD,4(R1)                                                    
         BAS   RE,ADJUST                                                        
         MVC   2(4,R9),LDEMNEW                                                  
         B     VALR56                                                           
*                                                                               
VALR54   MVI   LDEMS+1,C'R'                                                     
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALR54G                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
VALR54G  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS   GET ACTUAL RTG         
         XC    LDEMNEW,LDEMNEW                                                  
         MVC   LDEMNEW+2(2),4(R2)                                               
         MVC   LDEMOLD,LDEMVALS                                                 
         GOTO1 AADJCALC            CALCULATE ADJUSTMENT FOR NEW DEMO            
         BNE   VALRX                                                            
         MVC   2(4,R9),LDEMADJ                                                  
         OI    2(R9),X'80'                                                      
*                                                                               
VALR56   LA    R9,6(R9)                                                         
         MVI   0(R9),FF                                                         
*                                                                               
VALR58   LA    R2,6(R2)            NEXT DEMO OVERRIDE                           
         B     VALR46                                                           
*                                                                               
VALR60   LA    R0,LNDEMOS          PREPARE DEMO LIST FOR SPDEMUP                
         LA    R1,LDEMS                                                         
         LA    RE,ESTDEMS                                                       
*                                                                               
VALR62   OC    0(3,RE),0(RE)                                                    
         BZ    VALR64                                                           
         MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,VALR62                                                        
*                                                                               
VALR64   MVI   0(R1),X'FF'                                                      
         LA    RE,LDEMOVR          OVERRIDE ELEMENT LIST                        
         ST    RE,SPUPAOVR                                                      
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALR64G                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
VALR64G  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEMS,LDEMVALS    CALL SPDEMUP          
*                                                                               
VALR64K  LA    R2,NBRFSTEL                                                      
         TM    LINDS2,L2PRGCHG                                                  
         BO    VALR65                                                           
         TM    NBRSINDS,NBRSIPRG   PROGRAMMING OVERRIDE PREVIOUSLY?             
         BNZ   VALR65               - YUP, DON'T UPDATE PROGRAM NAME            
         TM    LINDS2,L2DTUCHG     DAYS/TIMES/UPGRADE CHANGED?                  
         BZ    VALR65                                                           
**       OC    NBRSPROG,SPACES                                                  
**       BH    VALR65                                                           
**       CLI   APACTN,ACTADD                                                    
**       BNE   VALR65                                                           
         MVC   NBRSPROG,SPACES                                                  
         MVC   NBRSPROG(L'SPUPPRG),SPUPPRG                                      
         LA    R0,L'NBRSPROG-2                                                  
         LA    R1,NBRSPROG+L'NBRSPROG-2                                         
         CLC   0(2,R1),=C'-S'                                                   
         BE    *+14                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-12                                                          
         B     VALR65                                                           
         MVC   0(2,R1),SPACES                                                   
*                                                                               
VALR65   MVC   REVPRG,NBRSPROG     RE-DISPLAY THE PROGRAMMING                   
         OI    REVPRGH+6,X'80'     RE-TRANSMIT THE PROGRAM NAME                 
         MVC   NBRSBOOK,SPUPFBK                                                 
*                                                                               
         CLI   APACTN,ACTADD       IF WE ADDING THEN WE NEED TO GET THE         
         BNE   VALR65Z                  NWS COMP RECORD                         
         L     R0,AIOAREA1                                                      
         L     RE,AIOAREA3                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     RF,AIOAREA1                                                      
         LA    R2,NBRFSTEL-NBRKEY(RF)                                           
         GOTO1 AGETCMPR,APPARM,NBRSDAYS,NBRSTIMS,NBRSSTA,LDEMVALS               
         BNE   VALR65Z             NONE, USE DEMOS WE HAVE ALREADY              
         L     R0,AIOAREA1                                                      
         L     RE,AIOAREA3                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R4,APELEM                                                        
         USING NBRDMELD,R4                                                      
         LA    R2,NBRDMDMO                                                      
         LA    R6,LDEMVALS                                                      
VALR65A  CLI   0(R2),X'FF'                                                      
         BE    VALR65Z                                                          
         TM    4(R2),X'80'         USER INPUT?                                  
         BNZ   *+14                                                             
         MVC   4(4,R2),0(R6)       NO, USE DEMO VALUE WE GOT                    
         B     *+10                                                             
         MVC   0(4,R6),4(R2)       YES, USER INPUT TAKES PRECEDENCE             
*                                                                               
         LA    R2,L'NBRDMDMO(R2)   NEXT DEMO                                    
         LA    R6,4(R6)                                                         
         B     VALR65A                                                          
*                                                                               
VALR65Z  LA    R4,APELEM                                                        
         USING NBRDMELD,R4                                                      
         LA    R2,NBRDMDMO         MOVE DEMO VALUES TO DEMO ELEMENT             
         LA    R6,LDEMVALS                                                      
*                                                                               
VALR66   CLI   0(R2),X'FF'         TEST FOR END                                 
         BE    VALR76                                                           
         OC    4(4,R2),0(R6)       DEMO VALUE (PRESERVING OVERRIDE BIT)         
         TM    LINDS,LAUTADJ       TEST FOR AUTO ADJUSTMENTS                    
         BZ    VALR75                                                           
         TM    4(R2),NBRDMOOV      YES - TEST THIS IS A MANUAL OVERRIDE         
         BO    VALR75                                                           
         LA    R8,LIMPVALS         NO - SEE IF THERE'S AN IMP ADJUST            
*                                                                               
VALR68   CLI   0(R8),FF                                                         
         BE    VALR72              NO                                           
         CLC   0(2,R8),1(R2)                                                    
         BNE   VALR70                                                           
         TM    2(R8),X'80'         YES - TEST DO THE ADJUSTMENT NOW             
         BO    *+14                                                             
         MVC   4(4,R2),2(R8)       NO - MOVE IN THE ADJUSTED DEMO VALUE         
         B     VALR75                                                           
         MVC   LDEMOLD,0(R6)       YES - DO THE ADJUSTMENT                      
         XC    LDEMADJ,LDEMADJ                                                  
         MVC   LDEMADJ+1(3),3(R8)                                               
         BAS   RE,ADJUST                                                        
         MVC   4(4,R2),LDEMNEW     ADJUSTED DEMO VALUE                          
         B     VALR75                                                           
*                                                                               
VALR70   LA    R8,6(R8)                                                         
         B     VALR68                                                           
*                                                                               
VALR72   TM    LINDS,LALLADJ       TEST FOR 'ALL' ADJUSTMENTS                   
         BZ    VALR75                                                           
         LA    R8,LALLVALS                                                      
*                                                                               
VALR73   CLI   0(R8),FF            SEE IF ADJUSTED DEM VALUE IS IN LIST         
         BE    VALR74                                                           
         CLC   0(2,R8),1(R2)                                                    
         BNE   *+14                                                             
         MVC   4(4,R2),2(R8)       YES - MOVE IN AUTO-ADJUSTED VALUE            
         B     VALR75                                                           
         LA    R8,6(R8)                                                         
         B     VALR73                                                           
*                                                                               
VALR74   MVC   LDEMADJ,LADJUST     NO - DO THE ADJUSTMENT WITH RAW RTG          
         MVC   LDEMOLD,0(R6)            ADJUST PCT                              
         BAS   RE,ADJUST                                                        
         MVC   4(4,R2),LDEMNEW                                                  
*                                                                               
VALR75   LA    R2,L'NBRDMDMO(R2)   NEXT DEMO                                    
         LA    R6,4(R6)                                                         
         B     VALR66                                                           
*                                                                               
VALR76   MVI   0(R2),0                                                          
         XC    LRTG,LRTG           SAVE PRIMARY DEMO VALUE                      
         MVC   LRTG+1(3),NBRDMDMO+5                                             
*                                                                               
         GOTO1 AADDELS,NBRRECD     ADD THE DEMO ELEMENT                         
*&&DO                                                                           
         L     R0,AIOAREA1                                                      
         L     RE,AIOAREA3                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     RF,AIOAREA1                                                      
         USING NBRKEY,RF                                                        
         LA    R2,NBRFSTEL                                                      
         DROP  RF                                                               
         GOTO1 APUTCMPR,APPARM,(X'C0',NBRSDAYS),NBRSTIMS,NBRSSTA,      X        
               APELEM,NBRSPROG                                                  
         L     R0,AIOAREA1                                                      
         L     RE,AIOAREA3                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*&&                                                                             
         XC    EBLOCK,EBLOCK       NOW REDISPLAY THE DEMO VALUES                
         MVI   EBTIN,C'B'                                                       
         MVI   EBLIN,4                                                          
         LA    R1,NBRDMDMO                                                      
         LA    R8,L'NBRDMDMO                                                    
         ZIC   R9,NBRDMLEN                                                      
         LA    R9,NBRDMEL(R9)                                                   
         BCTR  R9,0                                                             
         LA    R6,REVDM1H                                                       
*                                                                               
***  2 DECIMAL  ***                                                             
*ALR78   ST    R1,APPARM                                                        
*        LR    R0,R1               SAVE OFF R1                                  
*        GOTO1 AADJPREC,APPARM,,   SEE IF WE NEED TO CONVERT                    
*        LR    R1,R0               PUT IT BACK IN R1                            
***  2 DECIMAL  ***                                                             
VALR78   MVC   APFULL,4(R1)                                                     
         GOTO1 ADISDEM                                                          
         BXLE  R1,R8,*+8                                                        
         B     VALR80                                                           
         ZIC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         TM    FVATRB-FVIHDR(R6),FVAPROT                                        
         BO    *-10                                                             
         B     VALR78                                                           
*                                                                               
VALR80   TM    LINDS,LADDREC       ADDING THE NBR RECORD?                       
         BZ    VALR85                                                           
         TM    LINDS2,L2FRMDTL     FROM A NWS DETAIL?                           
         BZ    VALR85                                                           
         XC    APELEM,APELEM                                                    
         LA    R1,APELEM                                                        
         USING NBRDTELD,R1                                                      
         MVI   NBRDTEL,NBRDTELQ                                                 
         MVI   NBRDTLEN,NBRDTLNQ                                                
         MVC   NBRDTKEY,NWSDTLKY   SAVE THE NWS DETAIL KEY                      
         DROP  R1                                                               
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
VALR85   TM    LINDS,LCHG          ANY CHANGE MADE TO THE SCREEN?               
         BZ    VALRX               NONE, NOTHING TO WRITE THEN                  
*                                                                               
         TM    LINDS,LADDREC       ADDING THE NBR RECORD?                       
         BNZ   VALR87                                                           
*                                                                               
         NI    NBRCNTL,X'FF'-X'80' UNDELETE IF NECESSARY                        
         XC    IOKEY,IOKEY            WE HAVE TO GETREC AGAIN                   
         MVC   IOKEY(L'NBRKEY),NBRKEY                                           
         GOTO1 AIO,DIRRDD+IO1                                                   
         BE    *+14                                                             
         TM    IOERR,IOEDEL        RECORD IS DELETED?                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,FILGETU1+IORDEL   YES, GET NBR TO AIOAREA1                   
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALR87   L     R0,AIOAREA1                                                      
         L     RE,AIOAREA3                                                      
         LA    R1,2000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
****  NEWLY ADDED CREATION DATE AND CHANGE DATE ON BUY REVISION!                
****                        MHC  02/15/05                                       
         DROP  R3                                                               
         L     R1,AIOAREA1                                                      
         USING NBRKEY,R1                                                        
         LA    R2,NBRFSTEL                                                      
         USING NBRSELD,R2                                                       
         DROP  R1                                                               
         USING NBRKEY,R3                                                        
***  DON'T NEED R1 ANYMORE                                                      
*                                                                               
         TM    LINDS,LADDREC                                                    
         BNZ   VALR87E                                                          
****  CHANGING THE CHANGE DATE                                                  
         GOTO1 VDATCON,APPARM,(5,0),(3,NBRSCHDT)   CHANGE DATE                  
**                                                                              
         LA    R1,FILPUT1          PUT NBR RECORD                               
**                                                                              
         B     VALR87X                                                          
*                                                                               
VALR87E  DS    0H                                                               
****  ADDING THE CREATION DATE                                                  
         GOTO1 VDATCON,APPARM,(5,0),(3,NBRSCRDT)   CREATION DATE                
**                                                                              
         LA    R1,FILADD1          ADD NBR RECORD                               
**                                                                              
         TM    CMPOPTS,CAMODLY     IS IT DAILY CAMPAIGN?                        
         BZ    VALR87K                                                          
         OI    NBRSINDS,NBRSDALY   IT'S A DAILY CAMPAIN                         
VALR87K  CLC   BWSACT(3),=C'ADD'   WAS IT AN ADD ON SCREEN?                     
         BE    VALR87X              - YES, WE'RE DONE                           
***R87K  CLI   APACTN,ACTADD       WAS IT AN ADD ON SCREEN?                     
***      BE    VALR87X              - YES, WE'RE DONE                           
*                                                                               
         TM    LINDS2,L2FRMWRK     FROM WORK SKED (C'>')?                       
         BZ    VALR87N              - NOPE                                      
         OI    NBRSINDS,NBRSFWRK   FROM WORK                                    
         B     VALR87X                                                          
*                                                                               
VALR87N  TM    APINDS,APIOKADD     FROM BUY SKED (C'=')?                        
         BZ    VALR87X              - IT IS A REGULAR, NEW BUY REVISION         
         OI    NBRSINDS,NBRSDUPE                                                
         DROP  R2                                                               
*                                                                               
VALR87X  GOTO1 AIO                 ADDING OR PUTTING NOW                        
*                                                                               
         TM    LINDS2,L2RECDEL     RECORD WAS DELETED?                          
         BZ    VALR90                                                           
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'NBRKEY),NBRKEY                                           
*                                                                               
         GOTO1 AIO,DIRRDUP+IORDEL                                               
         BE    VALR90                                                           
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
VALR90   TM    LINDS,LADDREC                                                    
         BZ    VALR100                                                          
         LA    R2,IOKEY            BUILD HEADER POINTER TO FIND                 
         USING BWHKEY,R2                                                        
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM                                                     
         MVC   BWHKMKT,BMKT                                                     
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,DIRHI+IO2                                                    
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,FILGETU2                                                     
         L     R6,AIOAREA2                                                      
         LA    R6,BWHFSTEL-BWHKEY(R6)                                           
         XR    R0,R0                                                            
VALR92   CLI   0(R6),0             LOOK FOR INFO ELEMENT (X'06')                
         BE    VALR96              NEED TO ADD ONE THEN                         
         CLI   0(R6),INFELCDQ                                                   
         BE    VALR94                                                           
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VALR92                                                           
*                                                                               
         USING INFELD,R6                                                        
VALR94   TM    INFFLAG1,IFF1BYRV   ALREADY DOING REVISIONS?                     
         BNZ   VALR100             YES                                          
         OI    INFFLAG1,IFF1BYRV                                                
         B     VALR99                                                           
         DROP  R6                                                               
*                                                                               
VALR96   LA    R1,APELEM           SAVE ADDRESS WHERE TO PUT INFO ELEM          
         XC    APELEM,APELEM                                                    
         USING INFELD,R1                                                        
         MVI   INFELCD,INFELCDQ                                                 
         MVI   INFELLN,INFELLNQ                                                 
         OI    INFFLAG1,IFF1BYRV                                                
         DROP  R1                                                               
*                                                                               
         L     R6,AIOAREA2                                                      
         GOTO1 AADDELS,(R6)                                                     
*                                                                               
VALR99   LA    R1,FILPUT2          PUT NWH RECORD                               
         GOTO1 AIO                                                              
*                                                                               
VALR100  TM    LINDS2,L2FRMDTL                                                  
         BZ    VALRX                                                            
         MVC   IOKEY,NWSDTLKY                                                   
         GOTO1 AMIN,MINRD2                                                      
         BNE   VALRX                                                            
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         OI    BWDINDS2,BWDINBR    LINK ESTABLISHED                             
         GOTO1 AMIN,MINWRT2                                                     
         B     VALRX                                                            
*                                                                               
VALRERR  MVC   FVMSGNO,=AL2(1269)   NO NBR FOR CANADIANS                        
         CLC   FVMSGNO,=AL2(FVFOK)   SETS CONDITION JUST IN CASE                
*                                                                               
VALRX    B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO AUTO ADJUST A DEMO VALUE                                 *         
* INPUT  : LDEMOLD = DEMO VALUE                                       *         
*          LDEMADJ = PERCENT ADJUSTMENT                               *         
* OUTPUT : LDEMNEW = ADJUSTED DEMO VALUE                              *         
***********************************************************************         
         SPACE 1                                                                
ADJUST   NTR1                                                                   
         L     RE,LDEMOLD                                                       
***  2 DECIMAL                                                                  
         STCM  RE,8,APWORK+64                                                   
         NI    APWORK+64,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         ICM   RE,8,APWORK+64                                                   
***  2 DECIMAL                                                                  
         SR    RF,RF                                                            
         SRDA  RE,31                                                            
         M     RE,LDEMADJ                                                       
         D     RE,=F'1000'                                                      
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,LDEMNEW                                                       
***  2 DECIMAL                                                                  
         TM    LDEMOLD,X'40'       IS IT 2 DECIMAL?                             
         BZ    EXIT                 - NOPE                                      
         OI    LDEMNEW,X'40'                                                    
***  2 DECIMAL                                                                  
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO FIND DEMO ENTRY IN OLD DEMO ELEMENT                      *         
* INPUT  : LDEMS(3) = DEMO                                            *         
* OUTPUT : CC EQ 0 AND R1 = A(DEMO ENTRY)                             *         
*          CC NE 0 - DEMO NOT FOUND                                   *         
***********************************************************************         
         SPACE 1                                                                
GETOLD   LR    R0,RE                                                            
         OC    LDMOEL,LDMOEL                                                    
         BZ    GE10                                                             
         LA    R1,LDMOEL                                                        
         USING NBRDMELD,R1                                                      
         LA    RE,L'NBRDMDMO                                                    
         ZIC   RF,NBRDMLEN                                                      
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,NBRDMDMO                                                      
         CLC   1(2,R1),LDEMS+1                                                  
         BE    *+14                                                             
         BXLE  R1,RE,*-10                                                       
GE10     LTR   RE,R0                                                            
         B     *+8                                                              
         LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* MISC VALIDATAION ROUTINES                                           *         
***********************************************************************         
         SPACE 1                                                                
VALRCST  ST    RE,APFULL        ** VALIDATE COST **                             
         GOTO1 AFVAL                                                            
         BH    VALRX                                                            
         BE    *+14                                                             
         XC    0(L'NBRSCST1,R8),0(R8)                                           
         B     VALRCSTX                                                         
         CLC   REVC2NH,0(R1)       IS IT COST2 FIELD?                           
         BE    VALRCST5             - YUP, THERE IS NO EFFECTIVE DATE           
         CLI   APFLAG,1            COST FOUND -                                 
         BNE   *+14                 IF NO EFFECTIVE DATE, ERROR                 
         MVC   FVADDR,APDUB                                                     
         B     EMED                                                             
VALRCST5 ZIC   RF,FVILEN                                                        
         ST    RF,APPARM+4                                                      
         ST    R1,LSVREG1                                                       
         GOTO1 VCASHVAL,APPARM,FVIFLD                                           
         CLI   APPARM,X'FF'                                                     
         BE    EICO                                                             
         GOTO1 ANETCOST,APPARM+4   NET DOWN IF NECESSARY                        
         L     R1,LSVREG1                                                       
*******  CLI   APACTN,ACTADD       IF NOT ACTION=ADD                            
*******  BE    *+12                                                             
*******  TM    FVIIND-FVIHDR(R1),FVIVAL   AND PREVIOUSLY VALIDATED,             
*******  BO    VALRCSTX                   THEN AVOID DOUBLE NET DOWN            
         MVC   0(L'BWDCOST1,R8),APPARM+4                                        
*                                                                               
VALRCSTX L     RE,APFULL                                                        
         BR    RE                                                               
         SPACE 2                                                                
VALRDAT  ST    RE,APFULL        ** VALIDATE DATE **                             
         ST    R1,APDUB+4                                                       
         XC    0(L'NBRSEDT2,R8),0(R8)                                           
         CLI   5(R1),0                                                          
         BNE   VALRDAT2                                                         
         CLI   APFLAG,1            DATE NOT FOUND -                             
         BE    VALRDATX             IF PREVIOUS EMPTY DATE, OK                  
         MVI   APFLAG,1             ELSE                                        
         MVC   APDUB(4),APDUB+4        SAVE A(FIRST EMPTY DATE HDR)             
         B     VALRDATX                                                         
*                                                                               
VALRDAT2 GOTO1 AVALDAT                                                          
         BNE   VALRX                                                            
         CLI   APFLAG,1            DATE FOUND -                                 
         BNE   *+14                                                             
         MVC   FVADDR,APDUB         IF PREVIOUS EMPTY DATE, ERROR               
         B     EMED                                                             
         OC    APWORK+6(6),APWORK+6                                             
         BNZ   EIDT                ONLY ONE DATE PLEASE                         
         GOTO1 VDATCON,APPARM,APWORK,(3,0(R8))                                  
*                                                                               
VALRDATX L     RE,APFULL                                                        
         BR    RE                                                               
         SPACE 2                                                                
VALRCPP  SR    R0,R0            ** CALCULATE CPP **                             
         M     R0,=F'10'           IN: R1=COST                                  
         DR    R0,R8                   R8=RATING                                
         BR    RE                  OUT:R1=CPP                                   
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
DISKEY   GOTO1 ADISPKEY                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISREC   GOTO1 =A(DISPRECD),RR=APRELO                                           
         B     EXIT                                                             
***********************************************************************         
* DELETE RECORD                                                       *         
***********************************************************************         
         SPACE 1                                                                
DELREC   MVC   IOKEY,APRECKEY                                                   
         CLI   IOKEY,NBRKTYPQ                                                   
         BH    DELR9               DELETE PROHIBITED                            
         LA    R3,IOKEY                                                         
         USING NBRKEY,R3                                                        
         GOTO1 AIO,DIRRDUP+IO2                                                  
         BNE   DELRX                                                            
         CLC   IOKEY(L'BUYKEY),IOKEYSAV                                         
         BNE   DELRX                                                            
         OI    NBRKCNTL,X'80'      DELETE THE KEY                               
         GOTO1 AIO,DIRWRT+IO2                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AIO,FILGETU2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA2                                                      
DELR2    TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    DELR3                                                            
         GOTO1 ACHPTDOL            YES-CHANGE ACTUAL PTS/DOL                    
*                                                                               
DELR3    OI    NBRCNTL,X'80'                                                    
         GOTO1 AIO,FILPUT2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*&&DO                                                                           
         CLI   BWDKELPO,0          TEST PACKAGE/ORBIT RECORD                    
         BE    DELR8                                                            
         CLI   BWDKELSQ,0          YES-TEST PACKAGE/ORBIT SLAVE                 
         BNE   DELRX               YES-EXIT                                     
*                                  NO-THEN IT'S A PACKAGE/ORBIT MASTER          
         MVC   IOKEY,APRECKEY      DELETE ASSOCIATED SLAVES                     
         LA    R1,MINHI2                                                        
         B     DELR4+4                                                          
*                                                                               
DELR4    LA    R1,MINSEQ2                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DELR8                                                            
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   DELR8                                                            
         TM    TWAMODE,TWAMLSM     TEST LIST/SELECT MODE                        
         BZ    DELR6                                                            
         GOTO1 ACHPTDOL            YES-CHANGE ACTUAL PTS/DOL                    
*                                                                               
DELR6    GOTO1 AMIN,MINDEL                                                      
         B     DELR4               READ ALL PACKAGE RECORDS                     
*&&                                                                             
DELR8    MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DELRX                                                            
*                                                                               
DELR9    MVC   FVMSGNO,=AL2(FVNODEL)    DELETE PROHIBITED                       
*                                                                               
DELRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* OUTPUT : FVMSGNO NE FVFOK IF KEY IS INVALID                         *         
*          APRECKEY                                                   *         
*          APPARM FOR ROOT                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   DS    0H                                                               
*&&DO                                                                           
         XC    BWHKEY,BWHKEY       SET UP HEADER KEY                            
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
*                                                                               
         GOTO1 AVALMED,DUPMEDH     VALIDATE MEDIA                               
         BNE   VALPX                                                            
         MVC   BWHKAGMD,BAGYMD                                                  
*                                                                               
         GOTO1 AVALBYR,DUPBYRH     VALIDATE BUYER                               
         BNE   VALPX                                                            
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         OC    BYRPW,BYRPW                                                      
         BZ    VALP0                                                            
         GOTO1 AVALPWD                                                          
         BNE   VALPX                                                            
*                                                                               
VALP0    GOTO1 AVALCAM,DUPNUMH     VALIDATE CAMPAIGN NUMBER                     
         BNE   VALPX                                                            
         MVC   BWHKCAM,BCAM                                                     
*                                                                               
         GOTO1 AGETCLT,CMPCLTC     GET CLIENT                                   
         BE    *+16                                                             
         LA    R1,REVNUMH                                                       
         ST    R1,FVADDR                                                        
         B     VALPX                                                            
******                                                                          
VALP00   OI    REVC2KH+1,X'0C'     ZERO INTENSITY                               
         OI    REVC2NH+1,X'0C'       "      "                                   
         OI    REVC2NH+1,X'20'     PROTECT THE FIELD                            
         XC    REVC2N,REVC2N       JUST IN CASE IF ANYTHING IS HERE             
         MVI   REVC2NH+5,0                                                      
*                                                                               
         OI    REVC2KH+6,FVOXMT    TRANSMIT THESE 2 FIELDS                      
         OI    REVC2NH+6,FVOXMT                                                 
*                                                                               
         BRAS  RE,COS2CHK          CHECK IF WE NEED COS2 FIELD                  
******                                                                          
*                                                                               
         GOTO1 AGETPRD,CMPPRDN     GET PRODUCT                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEST,CMPESTN     GET CAMPAIGN ESTIMATE DETAILS                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AVALSTA,DUPSTAH     VALIDATE STATION                             
         BNE   VALPX                                                            
         MVC   BWHKMKT,BMKT        STATION'S MARKET                             
*                                                                               
         GOTO1 AVALDAY,DUPDAYH     VALIDATE DAYS                                
         BNE   VALPX                                                            
*                                                                               
         GOTO1 AVALTIM,DUPTIMH     VALIDATE TIMES                               
         BNE   VALKX                                                            
*                                                                               
         GOTO1 AVALDPL,DUPDPLH     VALIDATE DAYPART/LENGTH                      
         BNE   VALPX                                                            
         CLI   CMPDPOPT,C'M'       TEST SUBDPTS SCHEDULED UNDER MASTER          
         BNE   *+12                                                             
         CLI   DPTTYPE,C'S'        YES-TEST THIS IS A SUBDAYPART                
         BE    ESUB                    YES-ERROR                                
         CLI   BSLN,0              TEST FOR SPOT LENGTH                         
         BE    ENOSLN                                                           
*                                                                               
         MVI   LSUBDPT,0           SUB-DAYPART                                  
         MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,DUPSDPH                                                    
         BH    VALPX                                                            
         BL    VALP1                                                            
         CLI   CMPDPOPT,C'S'       TEST SCHEDULE SUBDPTS SEPERATELY             
         BE    EIIF                YES-INVALID INPUT FIELD                      
         MVC   LSUBDPT,FVIFLD                                                   
         LA    RE,L'DPTSUBS                                                     
         LA    RF,DPTSUBS                                                       
         CLC   FVIFLD(1),0(RF)                                                  
         BE    VALP1                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,*-14                                                          
         MVC   FVMSGNO,=AL2(FVIDPT)                                             
         B     VALPX                                                            
*                                                                               
VALP1    GOTO1 AIO,DIRHID+IO1      READ CAMPAIGN MARKET HEADER POINTER          
         BNE   VALPX                                                            
         CLC   BWHKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                  
         BNE   ERNF                                                             
         MVC   LHDRKEY,IOKEY       SAVE THE HEADER KEY                          
         MVC   LHDRDA,IODA         SAVE THE HEADER DISK ADDRESS                 
*                                                                               
         GOTO1 AIO,FILGET1         GET THE HEADER RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1         DOES THIS STATION ALREADY EXIST?             
         LA    R4,BWHFSTEL                                                      
         SR    R0,R0                                                            
         SR    RE,RE                                                            
*                                                                               
VALP2    CLI   0(R4),0                                                          
         BE    ERNF                                                             
         CLI   0(R4),BWHELCDQ                                                   
         BNE   *+18                                                             
         USING BWHEL,R4                                                         
         IC    RE,BWHSEQ                                                        
         CLC   BWHSTA,QSTA                                                      
         BE    VALP4                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALP2                                                            
*                                                                               
VALP4    XC    APRECKEY,APRECKEY                                                
         MVI   BWDKTYP,BWDKTYPQ    SET UP FIRST PART OF DETAIL KEY              
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BWHKSEQ     CAMPAIGN/MARKET SEQ NO                       
         MVI   BWDKELCD,1                                                       
         STC   RE,BWDKELST         STATION CODE                                 
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY,BDAYS                                                   
         MVC   BWDKELTM,PTIMES                                                  
*                                                                               
         MVC   APRECKEY+20(8),QSTA SET STATION/DPT/LEN IN KEY SO THAT           
         MVC   APRECKEY+28(1),BDPT ROOT CAN DETECT KEY CHANGE                   
         MVC   APRECKEY+29(1),BSLN                                              
*                                                                               
         LA    R1,DUPL1H           SET APPARM FOR ROOT                          
         ST    R1,APPARM                                                        
         MVI   APPARM+4,0                                                       
         LA    RF,DUPL2H                                                        
         SR    RF,R1                                                            
         STH   RF,APPARM+6                                                      
*&&                                                                             
VALPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST LIST SCREEN HOOK                                              *         
***********************************************************************         
         SPACE 1                                                                
FSTSCR   SR    R1,R1                                                            
         OC    INORTG,INORTG       TEST FOR RATING OPTION                       
         BZ    FSCR4                                                            
         LA    RE,ESTDEMS                                                       
*                                                                               
FSCR2    OC    0(3,RE),0(RE)       YES - VALIDATE IT                            
         BZ    ERTGOP                                                           
         CLC   1(2,RE),INORTG+1                                                 
         BE    FSCR4                                                            
         LA    RE,3(RE)                                                         
         BCT   R1,FSCR2                                                         
*                                                                               
FSCR4    LPR   R1,R1                                                            
         LA    R8,ESTDEMS                                                       
         GOTO1 AGETDEMS                                                         
         LA    RE,L'LDNAMES                                                     
         MR    R0,RE                                                            
         LA    R1,LDNAMES(R1)      POINT TO TARGET DEMO NAME                    
         MVC   DUPHED(L'LDNAMES),0(R1)   FORMAT DEMO NAME TO HEADLINE           
         OI    DUPHEDH+6,FVOXMT                                                 
*                                                                               
FSCRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   DS    0H                                                               
*&&DO                                                                           
         MVC   IOKEY(13),APRECKEY                                               
         LA    R1,MINSEQ2                                                       
         CLI   APINDS,APILNSEQ                                                  
         BE    GETS2                                                            
         LA    R1,MINHI2                                                        
*                                                                               
GETS2    GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETS99                                                           
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   GETS99                                                           
         L     R3,AIOAREA2                                                      
         CLC   BWDDPT,BDPT         MATCH DAYPART                                
         BNE   GETS3                                                            
         CLC   BWDSLN,BSLN         MATCH SPOT LENGTH                            
         BNE   GETS3                                                            
         CLI   LSUBDPT,0           TEST SUBDAYPART FILTER                       
         BE    GETS4                                                            
         CLC   BWDSUBDP,LSUBDPT    YES-MATCH SUBDAYPART                         
         BE    GETS4                                                            
*                                                                               
GETS3    LA    R1,MINSEQ2                                                       
         B     GETS2                                                            
*                                                                               
GETS4    XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(13),IOKEY                                               
         B     GETSX                                                            
*                                                                               
GETS99   MVI   APMODE,APMEOFS                                                   
*&&                                                                             
GETSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT RECORD                                          *         
***********************************************************************         
         SPACE  1                                                               
DISSEL   DS    0H                                                               
*&&DO                                                                           
         L     R5,APPARM                                                        
         USING REVL1H,R5                                                        
         L     R3,AIOAREA2                                                      
         LA    R1,REVCSTH          COST                                         
         LA    R8,BWDCOST1                                                      
         XC    EBLOCK,EBLOCK       SET UP EBLOCK FOR EDITING                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         MVI   EBFLOAT,C'$'                                                     
         BAS   RE,DISRCST                                                       
*                                                                               
         LA    R4,BWDEL            RATING                                       
         SR    R0,R0                                                            
*                                                                               
DISS2    CLI   0(R4),0                                                          
         BE    DISS6                                                            
         CLI   0(R4),DMOELCDQ                                                   
         BE    *+14                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DISS2                                                            
         USING DMOEL,R4                                                         
         LA    R1,DMODEMO          FIND TARGET RATING                           
         OC    INORTG,INORTG                                                    
         BZ    DISS4                                                            
         LA    RE,L'DMODEMO                                                     
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         CLC   1(2,R1),INORTG+1                                                 
         BE    DISS4                                                            
         BXLE  R1,RE,*-10                                                       
         B     DISS6                                                            
*                                                                               
***  2 DECIMAL  ***                                                             
*ISS4    ST    R1,APPARM                                                        
*        LR    R0,R1               SAVE OFF R1                                  
*        GOTO1 AADJPREC,APPARM,,   SEE IF WE NEED TO CONVERT                    
*        LR    R1,R0               PUT IT BACK IN R1                            
***  2 DECIMAL  ***                                                             
DISS4    MVC   APFULL,4(R1)                                                     
         LA    R6,DUPRTGH                                                       
         GOTO1 ADISDEM                                                          
         OI    DUPRTGH+6,FVOXMT                                                 
*                                                                               
DISS6    MVC   DUPPRG,NBRSPROG     PROGRAMMING                                  
         OI    DUPPRGH+6,FVOXMT                                                 
*                                                                               
         MVC   DUPCOD,BWDUCODE     USER CODE                                    
         OI    DUPCODH+6,FVOXMT                                                 
*                                                                               
         XC    DUPDAT,DUPDAT       DATES                                        
         OI    DUPDATH+6,FVOXMT                                                 
         OC    BWDDATES,BWDDATES                                                
         BZ    DISS8                                                            
         GOTO1 VDATCON,APPARM,(2,BWDDATES),(8,DUPDAT)                           
         MVI   DUPDAT+8,C'-'                                                    
         GOTO1 (RF),(R1),(2,BWDDATES+2),(8,DUPDAT+9)                            
*                                                                               
         DROP  R5                                                               
DISS8    L     R5,ATWA             RE-ESTABLISH TWA ADDRESSABILITY              
         USING TWAD,R5                                                          
*&&                                                                             
         B      EXIT                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST/SELECT DATA                                           *         
* INPUT  : APPARM(4)=A(TWA DISPLAY LINE)                              *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   DS    0H                                                               
*&&DO                                                                           
         L     R4,APPARM                                                        
         USING DUPL1H,R4                                                        
         MVC   IOKEY(13),APRECKEY  READ THE RECORD                              
         GOTO1 AMIN,MINRD2                                                      
         BE    *+14                                                             
         MVC   FVADDR,APPARM                                                    
         B     VALSX                                                            
         L     R3,AIOAREA2                                                      
         NI    LINDS,255-LCHG                                                   
         TM    DUPCODH+FVIIND-FVIHDR,FVIVAL  TEST USER CODE CHANGED             
         BO    VALS2                                                            
         MVC   APFULL,DUPCOD                                                    
         OC    APFULL,SPACES                                                    
         CLC   BWDUCODE,APFULL     TEST CODE CHANGED                            
         BE    VALS2                                                            
         MVC   BWDUCODE,APFULL     YES-REPLACE                                  
         OI    LINDS,LCHG                                                       
*                                                                               
VALS2    TM    DUPDATH+FVIIND-FVIHDR,FVIVAL  TEST DATES CHANGED                 
         BO    VALS4                                                            
         GOTO1 AVALDATE,DUPDATH    YES-VALIDATE DATES                           
         BNE   VALSX                                                            
         CLC   APFULL,BWDDATES     TEST ANY CHANGE                              
         BNE   *+14                                                             
         CLC   APHALF,BWDWKS                                                    
         BE    VALS4                                                            
         OI    LINDS,LCHG          YES                                          
         MVC   BWDDATES,APFULL     SET DATES                                    
         MVC   BWDWKS,APHALF       SET INACTIVE WEEKS                           
*                                                                               
VALS4    TM    LINDS,LCHG          TEST RECORD CHANGED                          
         BZ    VALSX                                                            
         GOTO1 AMIN,MINWRT2        YES-WRITE BACK RECORD                        
         BE    VALSX                                                            
         DC    H'0'                                                             
         DROP  R4                                                               
*&&                                                                             
VALSX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY COST ROUTINE                                                          
* R1 = A(FIELD HEADER)                                                          
* R8 = A(4-BYTE COST)                                                           
***********************************************************************         
DISRCST  NTR1                                                                   
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         OI    FVIIND-FVIHDR(R1),FVIVAL  SET PREVIOUSLY VALIDATED BIT           
*                                                                               
         MVI   EBFLOAT,0           RESET EBFLOAT JUST IN CASE                   
         MVI   EBDECS,2                                                         
         ST    R8,EBAIN                                                         
         LA    RE,L'FVIHDR(R1)                                                  
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'REVCS1                                                  
*                                                                               
         ICM   R4,15,0(R8)                                                      
         BNZ   *+14                                                             
         MVC   0(2,RE),=C'$0'                                                   
         B     DISRCSTX                                                         
*                                                                               
         SR    RE,RE                                                            
         TM    0(R8),X'80'         NEGATIVE?                                    
         BZ    DISRCST0                                                         
         MVI   EBFLOAT,C'-'        YES, SHOW THE MINUS SIGN                     
         L     RE,=X'FFFFFFFF'                                                  
*                                                                               
DISRCST0 LR    RF,R4                                                            
         D     RE,=F'100'          DIVISIBLE BY 100?                            
         LTR   RE,RE                                                            
         BZ    DISRCST1                                                         
*                                                                               
         C     RF,=F'100000'                                                    
         BL    DISRCST2                                                         
         MVI   EBDECS,0                                                         
         ST    RF,APDUB                                                         
         LA    RE,APDUB                                                         
         ST    RE,EBAIN                                                         
         B     DISRCST2                                                         
*                                                                               
DISRCST1 MVI   EBSCIN,X'82'        SO THAT '12.34' IS SHOWN AS '12'             
         MVI   EBDECS,0                                                         
*                                                                               
DISRCST2 GOTO1 VEDITOR,APPARM,EBLOCK                                            
         NI    EBOPT,X'FF'-X'40'         MINUS=YES                              
*                                                                               
DISRCSTX B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATE ROUTINE                                                *         
* R4 = A(FIELD HEADER)                                                *         
* R8 = A(3-BYTE DATE)                                                 *         
* R9 = RETURN ADDRESS                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISRDAT  GOTO1 VDATCON,APPARM,(3,0(R8)),(8,L'FVIHDR(R4))                        
         OI    6(R4),FVOXMT                                                     
         BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET DEMO NAMES AND FORMAT TO THE SCREEN                  *         
* INPUT : R8 = A(LIST OF 3-BYTE DEMO CODES)                           *         
***********************************************************************         
         SPACE 1                                                                
FMTDEMS  ST    RE,APFULL                                                        
         GOTO1 AGETDEMS            GET THE NAMES                                
         LA    R0,LNDEMOS          FORMAT DEMO NAMES TO SCREEN                  
         SR    R1,R1                                                            
         LA    RE,REVDN1H                                                       
         LA    RF,LDNAMES                                                       
*                                                                               
FMTD2    OC    0(7,RF),0(RF)                                                    
         BZ    FMTDX                                                            
         MVC   L'FVIHDR(L'REVDN1,RE),0(RF)                                      
         OI    6(RE),FVOXMT                                                     
         LA    RF,7(RF)                                                         
         BCT   R0,*+8                                                           
         B     FMTDX                                                            
*                                                                               
         AHI   RE,REVDM1X+L'REVDM1X-REVDN1H                                     
         B     FMTD2                                                            
*                                                                               
FMTDX    L     RE,APFULL                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ENOC     MVC   FVMSGNO,=AL2(FVFNONE)                                            
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
EIDT     MVC   FVMSGNO,=AL2(FVIDAT)                                             
         B     EXIT                                                             
EICO     MVC   FVMSGNO,=AL2(FVICST)                                             
         B     EXIT                                                             
EMED     MVC   FVMSGNO,=AL2(FVNOEFDT)                                           
         B     EXIT                                                             
EDLO     MVC   FVMSGNO,=AL2(FVDTLO)                                             
         B     EXIT                                                             
EDHI     MVC   FVMSGNO,=AL2(FVDTHI)                                             
         B     EXIT                                                             
EDLL     MVC   FVMSGNO,=AL2(FVDLL)                                              
         B     EXIT                                                             
EDIV     MVC   FVMSGNO,=AL2(FVIEFDT)                                            
         B     EXIT                                                             
EESD     MVC   FVMSGNO,=AL2(FVDEMES)                                            
         B     EXIT                                                             
ESUB     MVC   FVMSGNO,=AL2(FVISDPT)                                            
         B     EXIT                                                             
ENOSLN   MVC   FVMSGNO,=AL2(FVNOSLN)                                            
         B     EXIT                                                             
EINAWK   MVC   FVMSGNO,=AL2(FVINAWK)                                            
         B     EXIT                                                             
*                                                                               
EBADREP  MVC   FVMSGNO,=AL2(FVIREP)                                             
         LA    R1,REVREPNH                                                      
         B     EEXITR1                                                          
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         LA    R1,REVMEDH                                                       
         B     EEXITR1                                                          
EMAXSTA  MVC   FVMSGNO,=AL2(FVSTAOV)                                            
         LA    R1,REVSTAH                                                       
         B     EEXITR1                                                          
ECMSEQ   MVC   FVMSGNO,=AL2(FVCMSEQ)                                            
         LA    R1,REVNUMH                                                       
         B     EEXITR1                                                          
ETMD     MVC   FVMSGNO,=AL2(FVTMD)                                              
         LA    R1,REVMEDH                                                       
         B     EEXITR1                                                          
ENUE     MVC   FVMSGNO,=AL2(FVNOUPG)                                            
         LA    R1,REVUPGH                                                       
         B     EEXITR1                                                          
ERTGOP   MVC   FVMSGNO,=AL2(FVIRTGOP)                                           
         LA    R1,BWSOPTH                                                       
         B     EEXITR1                                                          
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
         B     EEXITR1                                                          
*                                                                               
EEXITR1  ST    R1,FVADDR                                                        
         B     EXIT                                                             
         EJECT                                                                  
FF       EQU   X'FF'                                                            
OVERELEM EQU   X'DE'                                                            
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
SPACES   DC    32C' '                                                           
*                                                                               
INPTTABL DS    0H                  LIST OF FIELD HEADERS WHOSE FIELDS           
         DC    AL2(REVDAYH-TWAD)     CAN HAVE INPUT THAT WE MIGHT NEED          
         DC    AL2(REVTIMH-TWAD)     TO PROTECT                                 
         DC    AL2(REVDPLH-TWAD)                                                
         DC    AL2(REVSDPH-TWAD)                                                
         DC    AL2(REVCS1H-TWAD)                                                
         DC    AL2(REVEF2H-TWAD)                                                
         DC    AL2(REVCS2H-TWAD)                                                
         DC    AL2(REVEF3H-TWAD)                                                
         DC    AL2(REVCS3H-TWAD)                                                
         DC    AL2(REVC2NH-TWAD)                                                
         DC    AL2(REVREPNH-TWAD)                                               
         DC    AL2(REVPRGH-TWAD)                                                
         DC    AL2(REVIDH-TWAD)                                                 
         DC    AL2(REVDATH-TWAD)                                                
         DC    AL2(REVUPGH-TWAD)                                                
         DC    AL2(REVDM1H-TWAD)                                                
         DC    AL2(REVDM1H-TWAD+1*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+2*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+3*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+4*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+5*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+6*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+7*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+8*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+9*(REVDM1X+L'REVDM1X-REVDN1H))                  
         DC    AL2(REVDM1H-TWAD+10*(REVDM1X+L'REVDM1X-REVDN1H))                 
         DC    AL2(REVDM1H-TWAD+11*(REVDM1X+L'REVDM1X-REVDN1H))                 
         DC    AL2(REVDM1H-TWAD+12*(REVDM1X+L'REVDM1X-REVDN1H))                 
         DC    AL2(REVDM1H-TWAD+13*(REVDM1X+L'REVDM1X-REVDN1H))                 
         DC    AL2(REVDM1H-TWAD+14*(REVDM1X+L'REVDM1X-REVDN1H))                 
         DC    AL2(REVCM1H-TWAD)                                                
         DC    AL2(REVCM2H-TWAD)                                                
         DC    AL2(REVCM3H-TWAD)                                                
         DC    AL2(REVCM4H-TWAD)                                                
         DC    AL2(REVCM5H-TWAD)                                                
         DC    AL2(0)                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IO ERROR BITS                                      *         
* IN : IOERR  - IO ERROR RETURN BYTE                                  *         
* OUT: APINDS - APPLICATION INDICATORS BYTE                           *         
*      CC     - EQ  OK                                                *         
*             - NE  NON RECOVERABLE ERROR                             *         
***********************************************************************         
         SPACE 1                                                                
IOCHECK  NTR1  BASE=*,LABEL=*                                                   
         TM    IOERR,IOEDSK        NON-RECOVERABLE DISK ERROR                   
         BO    IOCH99                                                           
*                                                                               
         MVI   APINDS,0                                                         
         OI    APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
*                                                                               
         TM    IOERR,IOEEOF        END-OF-FILE                                  
         BO    *+12                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND                             
         BZ    *+12                                                             
         NI    APINDS,FF-APIOKDIS-APIOKCHA-APIOKDEL                             
         OI    APINDS,APIOKADD                                                  
*                                                                               
         TM    IOERR,IOEDEL         RECORD IS DELETED                           
         BZ    *+12                                                             
         NI    APINDS,FF-APIOKADD-APIOKCHA-APIOKDEL                             
         OI    APINDS,APIOKRES                                                  
*                                                                               
         CR    RE,RE                                                            
         B     IOCHX                                                            
*                                                                               
IOCH99   MVC   FVMSGNO,=AL2(FVFIOER)    IO ERROR                                
         LA    R1,REVMEDH                                                       
         ST    R1,FVADDR                                                        
         LTR   RE,RE                                                            
*                                                                               
IOCHX    J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* MAKES COPIES OF THE DEMO AND SPOTS/WEEK ELEMENTS IN LDMOEL AND LSPWEL         
*                                                                               
* ON ENTRY:    AIOAREA3            BUY REVISION RECORD                          
*                                                                               
* ON EXIT:     LDMOEL              COPY OF THE DEMO ELEMENT                     
*              LSPWEL              COPY OF THE SPOTS/WEEK ELEMENT               
***********************************************************************         
CPYDMOSP NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
         LA    R2,NBRFSTEL                                                      
         XC    LDMOEL,LDMOEL                                                    
         XC    LSPWEL,LSPWEL                                                    
         SR    RE,RE                                                            
DMOSP10  CLI   0(R2),0                                                          
         BE    DMOSPX                                                           
         LA    R4,LDMOEL                                                        
         CLI   0(R2),NBRDMELQ                                                   
         BE    DMOSP30                                                          
         LA    R4,LSPWEL                                                        
         CLI   0(R2),NBRSPELQ                                                   
         BE    DMOSP30                                                          
DMOSP20  IC    RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     DMOSP10                                                          
*                                                                               
DMOSP30  IC    RE,1(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     DMOSP20                                                          
         MVC   0(0,R4),0(R2)                                                    
DMOSPX   B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*  REP CHECK                          MHC   06/16/04                  *         
***********************************************************************         
REPCHK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VCOLY,APPARM,0,X'D9000ABC'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),APPARM,(C'P',REVREPN),LREP                                  
         BNZ   REPNO                                                            
*                                                                               
         MVC   LTMPKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,C'R'                                                       
         MVC   IOKEY+1(1),QMED                                                  
         MVC   IOKEY+2(3),REVREPN                                               
         MVC   IOKEY+5(2),QAGY                                                  
         MVC   IOKEY+7(10),=10C'0'                                              
         L     R4,AIOAREA1                                                      
         GOTO1 AIO,IOSTAFIL+IOHI+IO1                                            
         CLC   IOKEY(7),0(R4)      DID WE FIND A MATCHING RECORD?               
         BNE   REPNO                - NOPE WE DIDN'T                            
*                                                                               
REPYES   CR    RE,RE               SET THE CONDITION                            
         B     REPCHKX                                                          
REPNO    CR    RE,RA                                                            
REPCHKX  MVC   IOKEY,LTMPKEY                                                    
         J     EXIT                                                             
***********************************************************************         
* COS2 CHECK                          MHC   06/08/04                  *         
***********************************************************************         
COS2CHK  NTR1  BASE=*,LABEL=*                                                   
         MVC   LTMPKEY,IOKEY       SAVE OFF THE IOKEY                           
***                                                                             
         LA    R4,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R4                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         GOTO1 AIO,FILRD1          READ CLIENT RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA1                                                      
         TM    COPT4,COP4TRD       TRADE?                                       
         BO    COS2CHK8             - YUP!  EQUAL ON EXIT                       
         TM    COPT3,COP3COSQ  OPTIONAL?                                        
         BNO   COS2NO               - NOPE, NOT EQUAL ON EXIT                   
         DROP  R4                                                               
*                                                                               
COS2CHK8 OI    LINDS2,L2COS2                                                    
         NI    REVC2KH+1,X'FF'-X'0C'   NORMAL INTENSITY                         
         NI    REVC2NH+1,X'FF'-X'0C'   NORMAL INTENSITY                         
         NI    REVC2NH+1,X'FF'-X'20'   TAKE OFF PROTECTION                      
         OI    REVC2KH+6,X'80'                                                  
         OI    REVC2NH+6,X'80'                                                  
*                                                                               
COS2YES  CR    RE,RE               SET THE CONDITION                            
         B     COS2CHKX                                                         
COS2NO   CR    RE,RA                                                            
         NI    LINDS2,X'FF'-L2COS2   TAKE OFF THIS FLAG                         
COS2CHKX MVC   IOKEY,LTMPKEY       RESTORE THE IOKEY                            
         J     EXIT                                                             
***********************************************************************         
* ROUTINE SETS THE LENGTH FOR FILLED FIELDS AFTER GOING THROUGH       *         
* DISPRECD.  SPECIFICLY DESIGNED FOR C'=' FROM LIST (DUP BUY RECORD)  *         
*                                   MHC   06/12/03                    *         
***********************************************************************         
SETLNTH  NTR1  BASE=*,LABEL=*                                                   
**** WE NEED TO CLEAR THE LINE AND SEQUENCE NUMBERS!!                           
         LA    R2,REVCS1H          START OFF WITH COST1 FIELD                   
*                                                                               
SETLN00  CLI   0(R2),0             IS THE LENGTH TOTAL 0?                       
         BE    SETLNX               - YA, GET OUTTA HERE                        
         TM    1(R2),X'20'         IS IT A PROTECTED FIELD?                     
         BZ    SETLN10              - NOPE                                      
         TM    1(R2),X'08'         IS IT HIGH INTENSITY?                        
         BZ    SETLNNXT             - NO, GO TO THE NEXT FIELD                  
         NI    1(R2),X'FF'-X'20'   TAKE OFF PROTECTED FIELD                     
SETLN10  XR    R0,R0                                                            
         IC    R0,0(R2)            R0 NOW HAS THE TOTAL LENGTH                  
         TM    1(R2),X'02'         WE HAVE EXTENDED FIELD HEADER?               
         BNO   *+8                                                              
         SHI   R0,8                TAKE OUT LENGTH OF EXTENDED FIELD            
         SHI   R0,8                TAKE OUT HEADER LENGTH                       
         LR    R3,R0               R3 HOLDS THE REAL LENGTH                     
         LA    R6,8(R2)            START OF FIELD                               
         SHI   R0,1                TO POINT R6 TO THE LAST BYTE                 
         AR    R6,R0                                                            
*                                                                               
SETLN20  CLI   0(R6),C' '          IS IT A SPACE OR LOWER?                      
         BH    SETLN50              - NOPE, GET OUTTA HERE                      
         BCTR  R6,0                MOVE TO CHECK FOR NON-SPACE/NULLS            
         BCT   R3,SETLN20                                                       
*                                                                               
SETLN50  STC   R3,5(R2)                                                         
*                                                                               
SETLNNXT XR    R0,R0               BUMP R2 TO POINT TO NEXT FIELD               
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     SETLN00                                                          
*                                                                               
SETLNX   J     EXIT                                                             
***********************************************************************         
* ROUTINE CHECKS IF THERE ARE ANY SPOTS WITHIN INACTIVE WEEKS         *         
* WHEN THE DATE FIELD IS FILLED IN, GIVES ERROR IF A SPOT IS FOUND    *         
*                                   MHC   04/28/03                    *         
***********************************************************************         
         SPACE 1                                                                
INAWKCHK NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA3         NEW DETAIL                                   
         USING NBRRECD,R3                                                       
         LA    R3,NBRFSTEL                                                      
         USING NBRSELD,R3                                                       
*                                                                               
         XC    APDUB,APDUB         GET THE INACTIVE WEEKS MASK                  
         MVC   APDUB(2),NBRSWKS                                                 
         MVC   APDUB+2(5),NBRSWKS2                                              
         OC    APDUB,APDUB         ANY BITS ON?                                 
         BZ    INAWKX              NONE, NO PROBLEM (WE ALREADY SET CC)         
         LM    R8,R9,APDUB                                                      
*                                                                               
INAWK10  CLI   0(R3),0             CAN'T FIND X'30'???                          
         BE    INAWKNS              - NO SPOTS FOUND AT ALL, NO PROB            
         CLI   0(R3),NBRSPELQ      X'30' SPOTS PER WEEK ELEMENT                 
         BE    INAWK20              - FOUND IT, WE'RE GOOD                      
         XR    R0,R0                - NOPE, BUMP                                
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     INAWK10                                                          
*                                                                               
         USING NBRSPELD,R3                                                      
INAWK20  XR    RE,RE                                                            
         ICM   RE,1,NBRSPLEN       COUNTER, GET DYNAMIC LENGTH                  
         SHI   RE,4                (NOT ALWAYS 14 WEEKS)                        
         LA    R3,NBRSPSPW         POINT R3 AT SPOTS PER FIRST WEEK             
*                                                                               
INAWK30  STCM  R8,2,APBYTE                                                      
         TM    APBYTE,X'80'        THIS WEEK IS MASKED?                         
         BZ    INAWK40                                                          
         CLI   0(R3),0             YES, ARE THERE ANY SPOTS?                    
         BNE   INAWKX                   YES, CONDITION SET TO !=                
INAWK40  LA    R3,1(R3)                                                         
         SLDL  R8,1                                                             
         BCT   RE,INAWK30                                                       
*                                                                               
INAWKNS  CR    RE,RE               CONDITION SET TO =                           
*                                                                               
INAWKX   XIT1                                                                   
         DROP  R3                                                               
***********************************************************************         
* DISPLAY RECORD                                                      *         
***********************************************************************         
DISPRECD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    BYRPW,BYRPW         TEST BUYER PASSWORD REQUIRED                 
         BZ    DISR0                                                            
         GOTO1 AVALPWD             YES-VALIDATE IT                              
         BNE   DISRX                                                            
*                                                                               
DISR0    OI    REVC2KH+1,X'0C'     ZERO INTENSITY                               
         OI    REVC2NH+1,X'0C'       "      "                                   
         OI    REVC2NH+1,X'20'     PROTECT THE FIELD                            
         XC    REVC2N,REVC2N       JUST IN CASE IF ANYTHING IS HERE             
         MVI   REVC2NH+5,0                                                      
*                                                                               
         OI    REVC2KH+6,FVOXMT    TRANSMIT THESE 2 FIELDS                      
         OI    REVC2NH+6,FVOXMT                                                 
*                                                                               
         BRAS  RE,COS2CHK          CHECK IF WE NEED COS2 FIELD                  
*                                                                               
DISR1    DS    0H                                                               
******   TWAXC REVDAYH                                                          
         SR    RE,RE                                                            
         LA    R1,REVDAYH                                                       
         LA    RF,4095(R1)                                                      
DISR1A   IC    RE,0(R1)                                                         
         TM    1(R1),X'20'         PROTECTED FIELD                              
         BO    DISR1X                                                           
         AHI   RE,-9                                                            
         TM    1(R1),X'02'         EXTENDED HEADER?                             
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         LTR   RE,RE                                                            
         BM    DISR1XX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         OI    6(R1),X'80'                                                      
         OI    4(R1),X'20'         VALIDATED PREVIOUSLY                         
         IC    RE,0(R1)                                                         
DISR1X   BXLE  R1,RE,DISR1A                                                     
DISR1XX  DS    0H                                                               
******                                                                          
         L     R3,AIOAREA2                                                      
         CLI   0(R3),NBRKTYPQ      BUY REVISION RECORD?                         
         BNE   DISR1Z                                                           
         L     R0,AIOAREA3                                                      
****     LA    R1,4000                                                          
         LHI   R1,6000                                                          
         L     RE,AIOAREA2                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     DISR20              YES, DISPLAY IT THEN                         
*                                                                               
         USING BUYKEY,R3                                                        
DISR1Z   XC    IOKEY,IOKEY         SEE IF WE HAVE A BUY REVISION RECORD         
         LA    R2,IOKEY                                                         
         USING NBRKEY,R2                                                        
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,BUYMSTA+2                                                
         MVC   NBRKKBUY,APRECKEY+BUYKBUY-BUYKEY                                 
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,DIRHI+IO3       NEED IT TO BE IO3!!  MHC 06/02/03            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'NBRKEY),IOKEYSAV                                         
         BE    DISR10                                                           
*                                                                               
******   GOTO1 =A(SETUPNBR),RR=APRELO   SETUP BUY REV REC IN AIOAREA3           
         GOTO1 ASTUPNBR,APPARM,AIOAREA2,AIOAREA3,DBLOCK                         
*                                                                               
         TM    LINDS,LNOTPOCM      BUY RECORD NOT PART OF THIS CAMP?            
         BZ    DISR20              IT IS OF THIS CAMPAIGN                       
DISR2    LA    R1,INPTTABL         NO, PROTECT THE INPUT FIELDS                 
DISR2A   SR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         BZ    DISR20                                                           
         AR    RE,R5                                                            
         OI    1(RE),X'20'                                                      
         LA    R1,2(R1)                                                         
         B     DISR2A                                                           
         DROP  R3                                                               
***********************************************************************         
* WHEN WE HAVE A BUY REVISION RECORD                                            
***********************************************************************         
DISR10   GOTO1 AIO,FILGET3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
***********************************************************************         
* WHEN WE HAVE A BUY REVISION RECORD                                            
***********************************************************************         
DISR20   L     R3,AIOAREA3                                                      
         USING NBRKEY,R3                                                        
*                                                                               
         BRAS  RE,CPYDMOSP                                                      
*                                                                               
         USING NBRSELD,R4                                                       
         LA    R4,NBRFSTEL         1ST ELEMENT SHOULD BE SAVED ELEM             
         GOTO1 AGETDAY,NBRSDAYS                                                 
         MVC   REVDAY,QDAYS                                                     
*                                                                               
         GOTO1 AGETTIM,NBRSTIMS                                                 
         MVC   REVTIM,QTIMES                                                    
*                                                                               
         MVC   REVDPL(1),NBRSDYPT                                               
         EDIT  (B1,NBRSSLN),(3,REVDPL+1),WRK=APWORK,DUB=APDUB,         X        
               ALIGN=LEFT                                                       
         MVC   REVSDP,NBRSSBDP                                                  
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         LA    R1,REVCS1H                                                       
         LA    R8,NBRSCST1                                                      
         BAS   RE,DISRCST                                                       
         OC    NBRSEDT2,NBRSEDT2   EFFECTIVE DATE 2                             
         BZ    DISR25                                                           
         LR    R0,R4                                                            
         LA    R8,NBRSEDT2                                                      
         LA    R4,REVEF2H                                                       
         BAS   R9,DISRDAT                                                       
         LR    R4,R0                                                            
         LA    R1,REVCS2H          COST 2                                       
         LA    R8,NBRSCST2                                                      
         BAS   R9,DISRCST                                                       
         OC    NBRSEDT3,NBRSEDT3   EFFECTIVE DATE 3                             
         BZ    DISR25                                                           
         LR    R0,R4                                                            
         LA    R8,NBRSEDT3                                                      
         LA    R4,REVEF3H                                                       
         BAS   R9,DISRDAT                                                       
         LR    R4,R0                                                            
         LA    R1,REVCS3H          COST 3                                       
         LA    R8,NBRSCST3                                                      
         BAS   R9,DISRCST                                                       
*                                                                               
DISR25   OC    NBRSREP,NBRSREP     ANYTHING HERE?                               
         BZ    DISR25E              - NOPE, SKIP THIS                           
         GOTO1 VCOLY,APPARM,0,X'D9000ABC'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),APPARM,(C'U',NBRSREP),REVREPN                               
         B     DISR25X                                                          
*                                                                               
DISR25E  OC    LESTREP,LESTREP                                                  
         BZ    DISR27                                                           
         MVC   REVREPN,LESTREP     LESTREP COULD BE EMPTY                       
DISR25X  OI    REVREPNH+6,FVOXMT                                                
*                                                                               
DISR27   MVC   REVPRG,NBRSPROG                                                  
***      MVC   REVCOD,NBRSUCOD                                                  
         OC    NBRSDTES,NBRSDTES                                                
         BZ    DISR27X                                                          
         GOTO1 VDATCON,APPARM,(2,NBRSDTES),(8,REVDAT)                           
         MVI   REVDAT+8,C'-'                                                    
         GOTO1 (RF),(R1),(2,NBRSDTES+2),(8,REVDAT+9)                            
*                                                                               
DISR27X  LA    R4,NBRFSTEL                                                      
DISR30   CLI   0(R4),0                                                          
         BE    DISR30X                                                          
         CLI   0(R4),NBRIDELQ      ID ELEMENT                                   
         BNE   *+14                                                             
         USING NBRIDEL,R4                                                       
         MVC   REVID,NBRIDID                                                    
         B     DISR30LP                                                         
*                                                                               
         CLI   0(R4),NBRUPELQ      UPGRADE ELEMENT                              
         BNE   *+14                                                             
         USING NBRUPELD,R4                                                      
         MVC   REVUPG,NBRUPINP                                                  
         B     DISR30LP                                                         
*                                                                               
         CLI   0(R4),NBRCMELQ      COMMENT ELEMENT                              
         BNE   DISR35                                                           
         USING NBRCMELD,R4                                                      
         ZIC   R1,NBRCMLIN                                                      
         BCTR  R1,0                                                             
         MHI   R1,REVCM2H-REVCM1H                                               
         LA    R1,REVCM1H(R1)                                                   
         ZIC   RE,NBRCMLEN                                                      
         AHI   RE,(-1*(NBRCMOVQ+1))                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),NBRCMCOM                                                 
         OI    6(R1),X'80'                                                      
         B     DISR30LP                                                         
*                                                                               
DISR35   CLI   0(R4),NBRDMELQ      DEMO ELEMENT                                 
         BNE   DISR40                                                           
         USING NBRDMELD,R4                                                      
         ZIC   R9,NBRDMLEN                                                      
         BCTR  R9,0                                                             
         LA    R2,REVDM1H                                                       
         LA    R3,ESTDEMS                                                       
*                                                                               
DISR35A  LA    R1,NBRDMDMO         GO THRU OUR LIST FOR EST DEMOS               
         LA    RE,L'NBRDMDMO       SET UP BXLE LOOP                             
         LA    RF,NBRDMDMO(R9)                                                  
         XC    APDUB,APDUB                                                      
         XC    APFULL,APFULL                                                    
         MVI   EBFLOAT,0                                                        
*                                                                               
DISR35B  CLC   1(2,R1),1(R3)       FOUND A MATCH?                               
         BE    DISR35C             YES, DISPLAY THE DEMO THEN                   
         BXLE  R1,RE,DISR35B                                                    
         B     DISR35D             CAN'T FIND THIS PARTICULAR ONE               
*                                                                               
DISR35C  ST    R1,APPARM                                                        
***  2 DECIMAL  ***                                                             
*        LR    R0,R1               SAVE OFF R1                                  
*        GOTO1 AADJPREC,APPARM,,   SEE IF WE NEED TO CONVERT                    
*        LR    R1,R0               PUT IT BACK IN R1                            
***  2 DECIMAL  ***                                                             
         MVC   APFULL,4(R1)                                                     
*****  I HAVE NO IDEA WHY DISDEM ISN'T USED BEFORE NOW  *****                   
*****  R2 HAS THE HEADER FIELD INSTEAD OF R6                                    
         LR    R0,R6               (JUST IN CASE, SHOULDN'T NEED TO)            
         LR    R6,R2               NOW R6 HAS FIELD HEADER                      
         GOTO1 ADISDEM                                                          
         LR    R6,R0               RESTORE R6                                   
*&&DO                                                                           
         TM    APFULL,DMODEMOV     MANUAL OVERRIDE?                             
         BZ    *+12                                                             
         MVI   EBFLOAT,C'*'        YES, SHOW AS *999.9                          
*                                                                               
         NI    APFULL,X'FF'-DMODEMOV                                            
         LA    RF,APFULL                                                        
         ST    RF,EBAIN                                                         
         MVI   EBLIN,4                                                          
         LA    RF,8(R2)                                                         
         ST    RF,EBAOUT                                                        
         MVI   EBLOUT,L'REVDM1                                                  
         MVI   EBDECS,1                                                         
         MVI   EBSCIN,0                                                         
         CLI   1(R3),C'I'          TEST FOR IMPRESSION                          
         BNE   *+12                                                             
         MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
         GOTO1 VEDITOR,APPARM,EBLOCK                                            
         OI    6(R2),FVOXMT                                                     
*&&                                                                             
*                                                                               
DISR35D  LA    R3,3(R3)                                                         
         OC    0(3,R3),0(R3)       ANY MORE DEMOS?                              
         BZ    DISR30LP            NO, NEXT ELEMENT THEN                        
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,REVCM1H                                                       
         CR    R2,R0                                                            
         BL    DISR35A                                                          
*                                                                               
DISR40   CLI   0(R4),NBRC2ELQ      COST 2 OVERRIDE ELEMENT?                     
         BNE   DISR50               - NOPE                                      
         USING NBRC2ELD,R4                                                      
         LA    R1,REVC2NH                                                       
         LA    R8,NBRC2CST                                                      
         BAS   R9,DISRCST                                                       
*                                                                               
DISR50   DS    0H                                                               
*                                                                               
DISR30LP ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DISR30                                                           
*                                                                               
DISR30X  DS    0H                                                               
         DROP  R4                                                               
*                                                                               
DISRX    B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTENTION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,**B36X**,RA                                                    
         L     RC,APALOCAL                                                      
         L     R5,ATWA                                                          
         USING TWAD,R5                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     VALDATES                                                         
         B     VALRDAY                                                          
         B     CHPTSDOL                                                         
         B     GETDEMS                                                          
         B     DISDEM                                                           
         B     ADJCALC                                                          
         B     DISPKEY                                                          
*                                                                               
XIT      CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATES                                                                
* INPUT  : R1=A(DATES FIELD HEADER)                                             
* OUTPUT : APFULL=PACKED DATES START/END                                        
*          APHALF=INACTIVE WEEKS MASK                                           
*          APDUB =INACTIVE WEEKS MASK 2                                         
*                                                                               
* NOTE: APDUB WAS ALREADY CLOBBERED IN THIS ROUTINE SO NOW I'M USING TO         
*        STORE THE ADDITIONAL BITS FOR MASK2                                    
***********************************************************************         
         SPACE 1                                                                
VALDATES XC    APFULL,APFULL                                                    
         XC    APHALF,APHALF                                                    
         CLI   FVILEN-FVIHDR(R1),0 TEST DATES ABSENT                            
         BE    VALDX                                                            
         GOTO1 AVALDAT             VALIDATE DATES                               
         BNE   VALDX                                                            
         MVI   FVINDX,0                                                         
         OC    APWORK+6(6),APWORK+6  CHECK FOR SECOND DATE                      
         BNZ   *+10                                                             
         MVC   APWORK+6(6),APWORK  IF NONE, COPY FIRST DATE                     
*                                                                               
         CLC   APWORK(6),APWORK+6  CHECK START LE END                           
         BH    VALD8                                                            
         GOTO1 VDATCON,APPARM,(3,CMPST),APDUB  CHECK DATES WITHIN               
         CLC   APWORK(6),APDUB                 CAMPAIGN PERIOD                  
         BL    VALD7                                                            
         GOTO1 (RF),(R1),(3,CMPND),APDUB                                        
         CLC   APWORK+6(6),APDUB                                                
         BH    VALD7                                                            
         GOTO1 VDATCON,APPARM,APWORK,(2,APFULL)                                 
         GOTO1 (RF),(R1),APWORK+6,(2,APFULL+2)                                  
*                                                                               
         XC    APDUB,APDUB         CAN USE APDUB(7) NOW FOR ADDITIONAL          
         L     R4,ATWA                                                          
         AHI   R4,CMPDATSP-TWAD                                                 
*                                                                               
         LA    R0,53               53 DAYS/WEEKS IS ABSOLUTE MAX                
         SR    RE,RE                                                            
         LA    R1,1                                                             
         SLL   R1,31                                                            
         MVI   APBYTE,0                                                         
*                                                                               
VALD2    CLI   APBYTE,2            TEST BEYOND DATES                            
         BE    VALD4                                                            
         CLI   0(R4),X'FF'         TEST END OF FLIGHT WEEKS                     
         BNE   *+12                                                             
         MVI   APBYTE,2            YES-FORCE INACTIVE                           
         B     VALD4                                                            
         CLI   APBYTE,0            TEST BEFORE DATES                            
         BNE   VALD3                                                            
         CLC   APFULL(2),2(R4)     YES-TEST START DATE AFTER THIS WEEK          
         BH    VALD4                   YES-STILL BEFORE DATES                   
         MVI   APBYTE,1                NO-DURING DATES                          
*                                                                               
VALD3    CLC   APFULL+2(2),0(R4)    DURING DATES-TEST END DATE BEFORE           
         BNL   VALD5                             THIS WEEK                      
         MVI   APBYTE,2             YES-BEYOND DATES NOW                        
*                                                                               
VALD4    MVI   APDUB+7,X'80'       INACTIVE WEEK                                
         B     *+8                                                              
VALD5    MVI   APDUB+7,X'00'       ACTIVE WEEK                                  
*                                                                               
         LM    RE,RF,APDUB                                                      
         SLDL  RE,1                                                             
         STM   RE,RF,APDUB                                                      
         LA    R4,4(R4)                                                         
         BCT   R0,VALD2                                                         
*                                                                               
         LM    RE,RF,APDUB                                                      
         SLDL  RE,3                   MAXWKS=53, SO SHIFT FOR 56 BITS           
         STCM  RE,12,APHALF                                                     
         XC    APDUB,APDUB                                                      
         STCM  RE,3,APDUB                                                       
         STCM  RF,14,APDUB+2                                                    
         B     VALDX                                                            
*                                                                               
VALD7    MVC   FVMSGNO,=AL2(FVIDTCAM) DATES NOT WITHIN CAMPAIGN PERIOD          
         B     VALDX                                                            
*                                                                               
VALD8    MVC   FVMSGNO,=AL2(FVIDAT)   INVALID DATE FORMAT                       
         B     VALDX                                                            
*                                                                               
VALD9    MVC   FVMSGNO,=AL2(FVNOEDT)  END DATE MISSING                          
*                                                                               
VALDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE THE DAY OF AN EFFECTIVE DATE                    *         
* INPUT  : R8 = A(PACKED DATE)                                        *         
* OUTPUT : CC EQ OK                                                   *         
*             NE ERROR                                                *         
*          APPARM(1) = DAY OF INPUT DATE                              *         
*          APWORK(6) = MONDAY OF WEEK OF INPUT DATE                   *         
***********************************************************************         
         SPACE 1                                                                
VALRDAY  DS    0H                                                               
         GOTO1 VDATCON,APPARM,(3,(R8)),(0,APWORK)                               
         GOTO1 VGETDAY,APPARM,APWORK,APWORK+6                                   
         CLC   APWORK+6(3),BLANKS                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APBYTE,APPARM                                                    
         ZIC   RE,APBYTE                                                        
         CLI   ESTOWSDY,1          TEST OUT-OF-WEEK RORATORS                    
         BNH   VALRD2                                                           
         ZIC   RF,ESTOWSDY                                                      
         SR    RE,RF                                                            
         BZ    VALRDEQ                                                          
         BP    *+8                                                              
         LA    RE,7(RE)                                                         
         LNR   RE,RE                                                            
         B     VALRD4                                                           
*                                                                               
VALRD2   BCTR  RE,0                                                             
         LNR   RE,RE                                                            
         BZ    VALRDEQ             MONDAY IS OK                                 
*                                                                               
VALRD4   ST    RE,APPARM+8                                                      
         GOTO1 VADDAY,APPARM,APWORK,APWORK+6                                    
         MVC   APWORK(6),APWORK+6                                               
         ZIC   RE,APBYTE                                                        
         LA    RF,8                                                             
         SR    RF,RE                                                            
         LA    RE,1                                                             
         B     *+8                                                              
         SLL   RE,1                                                             
         BCT   RF,*-4                                                           
         IC    RF,BDAYS                                                         
         NR    RE,RF                                                            
         BZ    VALRDNE                                                          
         SLL   RE,1                                                             
         CLI   ESTOWSDY,1          TEST OUT-OF-WEEK-ROTATOR                     
         BNH   VALRD6                                                           
         ZIC   R0,ESTOWSDY         YES-ELIMINATE DAYS PREVIOUS                  
         LR    R1,R0                   TO WEEK START DAY                        
         SLL   RF,24                                                            
         SLL   RF,1                                                             
         BCT   R1,*-4                                                           
         SRL   RF,1                                                             
         BCT   R0,*-4                                                           
         SRL   RF,24                                                            
*                                                                               
VALRD6   CR    RE,RF                                                            
         BNH   VALRDNE                                                          
*                                                                               
VALRDEQ  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALRDX                                                           
VALRDNE  MVC   FVMSGNO,=AL2(FVIEFDT)                                            
VALRDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE THE TOTAL ACTUAL POINTS AND DOLLARS,              *         
* FOR ACTION=DELETE IN LIST/SELECT MODE                               *         
***********************************************************************         
         SPACE 1                                                                
CHPTSDOL L     R3,AIOAREA2                                                      
         USING NBRKEY,R3                                                        
         USING SAVAREA,R6                                                       
         L     R6,LASAVE                                                        
         NI    LINDS,255-LPKGSLV                                                
*&&DO                                                                           
         CLI   BWDKELPO,0          TEST PACKAGE SLAVE                           
         BE    *+16                                                             
         CLI   BWDKELSQ,0                                                       
         BE    *+8                                                              
         OI    LINDS,LPKGSLV       YES                                          
*&&                                                                             
         XC    APDUB,APDUB                                                      
         SR    R0,R0                                                            
         LA    R4,NBRFSTEL         SCAN ELEMENTS                                
         LR    R3,R4                                                            
         USING NBRSELD,R3                                                       
*                                                                               
CHPD1    CLI   0(R4),0                                                          
         BE    CHPD2                                                            
         CLI   0(R4),NBRDMELQ                                                   
         BNE   *+8                                                              
         ST    R4,APDUB            APDUB(4)=A(DEMO ELEMENT)                     
         CLI   0(R4),NBRSPELQ                                                   
         BNE   *+8                                                              
         ST    R4,APDUB+4          APDUB+4(4)=A(SPTS/WEEK ELEMENT)              
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CHPD1                                                            
*                                                                               
CHPD2    OC    APDUB+4(4),APDUB+4  TEST ANY SCHEDULING                          
         BZ    CHPDX               NO-EXIT                                      
         ICM   R4,15,APDUB         TEST DEMO ELEMENT                            
         BZ    CHPD10              NO                                           
         USING NBRDMELD,R4                                                      
         LA    R1,NBRDMDMO                                                      
         OC    INORTG,INORTG       TEST FOR OVERRIDE TARGET DEMO                
         BZ    CHPD4               NO - THEN TARGET = PRIMARY                   
         LA    RE,L'NBRDMDMO       YES - FIND TARGET                            
         ZIC   RF,NBRDMLEN                                                      
         AR    RF,R4                                                            
         BCTR  RF,0                                                             
         CLC   1(2,R1),INORTG+1                                                 
         BE    CHPD4                                                            
         BXLE  R1,RE,*-10                                                       
         B     CHPD10                                                           
         DROP  R4                                                               
*                                                                               
CHPD4    MVC   LDEMOLD,4(R1)       TARGET RATING                                
         NI    LDEMOLD,FF-NBRDMOOV                                              
***  2 DECIMAL                                                                  
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BZ    CHPD4E                                                           
         MVC   APWORK+64(4),LDEMOLD                                             
         NI    APWORK+64,FF-DMODEM2D                                            
         SR    R0,R0                                                            
         L     R1,APWORK+64                                                     
         D     R0,=F'10'           MAKE IT ONE DECIMAL VALUE                    
         ST    R1,APWORK+64                                                     
***  2 DECIMAL                                                                  
CHPD4E   L     R4,APDUB+4          R4=A(SPOTS/WEEK ELEMENT)                     
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         LA    R4,NBRSPSPW-NBRSPELD(R4)                                         
         LA    R8,SVACPTS          RE-CALCULATE WEEKLY ACTUAL POINTS            
         L     R5,ATWA                                                          
         AHI   R5,SVPKAPTS-TWAD                                                 
         ZIC   R9,CMPNWKS                                                       
*                                                                               
CHPD6    ZIC   R1,0(R4)                                                         
         SR    R0,R0                                                            
***      M     R0,LDEMOLD          SPOTS X RATING                               
         M     R0,APWORK+64        SPOTS X RATING                               
         L     R0,0(R8)                                                         
         SR    R0,R1                                                            
         ST    R0,0(R8)                                                         
         TM    LINDS,LPKGSLV                                                    
         BZ    *+14                                                             
         L     R0,0(R5)                                                         
         SR    R0,R1                                                            
         ST    R0,0(R5)                                                         
         LA    R4,1(R4)                                                         
         CR    R4,RF                                                            
         BNL   CHPD10                                                           
         LA    R8,4(R8)                                                         
         LA    R5,4(R5)                                                         
         BCT   R9,CHPD6                                                         
*                                  RE-CALCULATE ACTUAL DOLLARS                  
CHPD10   XC    APFULL,APFULL                                                    
         OC    NBRSEDT2,NBRSEDT2                                                
         BZ    CHPD11                                                           
         GOTO1 VDATCON,APPARM,(3,NBRSEDT2),(2,APFULL)                           
         OC    NBRSEDT3,NBRSEDT3                                                
         BZ    CHPD11                                                           
         GOTO1 VDATCON,APPARM,(3,NBRSEDT3),(2,APFULL+2)                         
*                                                                               
CHPD11   L     R4,APDUB+4          R4=A(SPOTS/WEEK ELEMENT)                     
         ZIC   R8,1(R4)                                                         
         AR    R8,R4                                                            
         L     R2,ATWA                                                          
         AHI   R2,CMPDATSP-TWAD                                                 
         LA    R4,NBRSPSPW-NBRSPELD(R4)                                         
         ZIC   R9,CMPNWKS                                                       
         ICM   RE,15,NBRSCST1      RE=COST                                      
*                                                                               
         L     RF,ATWA                                                          
         AHI   RF,SVPKADOL-TWAD                                                 
         L     R5,0(RF)                                                         
         L     RF,SVACDOL                                                       
*                                                                               
CHPD12   OC    APFULL+2(2),APFULL+2                                             
         BZ    CHPD14                                                           
         CLC   APFULL+2(2),2(R2)                                                
         BH    CHPD14                                                           
         ICM   RE,15,NBRSCST3                                                   
         B     CHPD16                                                           
*                                                                               
CHPD14   OC    APFULL(2),APFULL                                                 
         BZ    CHPD16                                                           
         CLC   APFULL(2),2(R2)                                                  
         BH    CHPD16                                                           
         ICM   RE,15,NBRSCST2                                                   
*                                                                               
CHPD16   SR    R0,R0                                                            
         ZIC   R1,0(R4)                                                         
         MR    R0,RE                                                            
         SR    RF,R1                                                            
         TM    LINDS,LPKGSLV                                                    
         BZ    *+6                                                              
         SR    R5,R1                                                            
         LA    R2,4(R2)            NEXT WEEK                                    
         LA    R4,1(R4)                                                         
         CR    R4,R8                                                            
         BNL   *+8                                                              
         BCT   R9,CHPD12                                                        
         ST    RF,SVACDOL                                                       
         TM    LINDS,LPKGSLV                                                    
         BZ    CHPD18                                                           
         L     RF,ATWA                                                          
         AHI   RF,SVPKADOL-TWAD                                                 
         ST    R5,0(RF)                                                         
*                                                                               
CHPD18   L     R5,ATWA                                                          
         OI    TWAFLAG,TWAFMTPD    INDICATE FORMAT ACTUAL PTS/DOL               
*                                                                               
CHPDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET DEMO NAMES VIA CALL TO DEMOCON                       *         
* INPUT : R8 = A(LIST OF 3-BYTE DEMO CODES)                           *         
***********************************************************************         
         SPACE 1                                                                
GETDEMS  DS    0H                                                               
         XC    DBLOCK,DBLOCK       GET DEMO NAMES                               
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         MVI   APBYTE,LNDEMOS                                                   
         ICM   R8,8,APBYTE                                                      
         ST    R8,APPARM                                                        
         LA    R0,ESTUSRNM                                                      
         XC    LDNAMES(7*LNDEMOS),LDNAMES                                       
         GOTO1 VDEMOCON,APPARM,,(2,LDNAMES),(C'S',DBLOCK),(R0)                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEMO VALUE ROUTINE                                          *         
* R6 = A(FIELD HEADER)                                                *         
* APFULL = 4-BYTE DEMO VALUE                                          *         
***********************************************************************         
         SPACE 1                                                                
DISDEM   DS    0H                                                               
         MVI   EBDECS,1                                                         
***  2 DECIMAL  ***                                                             
         TM    APFULL,NBRDMO2D     WE NEED 2 DECIMAL?                           
         BNO   *+8                                                              
         MVI   EBDECS,2                                                         
***  2 DECIMAL  ***                                                             
         MVI   EBSCIN,0                                                         
         MVI   EBFLOAT,0                                                        
         MVI   EBALIGN,C'L'                                                     
         MVI   EBOPT,0                                                          
         LA    R1,L'FVIHDR(R6)                                                  
         ST    R1,EBAOUT                                                        
         MVI   EBLOUT,L'REVDM1                                                  
         LA    RF,APFULL                                                        
***  2 DECIMAL  ***                                                             
         NI    APFULL,FF-DMODEM2D   TAKE OFF THE BIT FOR CORRECT AMOUNT         
***  2 DECIMAL  ***                                                             
         TM    APFULL,DMODEMOV                                                  
         BZ    DISDEM2                                                          
         MVI   EBFLOAT,C'*'        DEMO OVERRIDE GETS A *                       
         NI    APFULL,FF-DMODEMOV                                               
         OC    APFULL,APFULL       TEST DEMO = 0                                
         BNZ   DISDEM2                                                          
         MVC   0(6,R1),=C'  *0.0'  YES-EDIT MYSELF                              
***  2 DECIMAL  ***                                                             
         CLI   EBDECS,2            ARE WE DOING 2 DECIMALS?                     
         BNE   *+10                 - NOPE                                      
         MVC   0(6,R1),=C' *0.00'  YES-EDIT MYSELF                              
***  2 DECIMAL  ***                                                             
         B     DISDEM6                                                          
*                                                                               
DISDEM2  ST    RF,EBAIN                                                         
***  2 DECIMAL  ***                                                             
         CLI   EBDECS,2            ARE WE DOING 2 DECIMALS?                     
         BE    DISDEM2E             - YUP WE ARE                                
***  2 DECIMAL  ***                                                             
         CLC   APFULL,=F'10000'                                                 
         BL    DISDEM4                                                          
         CLI   EBFLOAT,C'*'                                                     
         BE    *+14                                                             
         CLC   APFULL,=F'100000'                                                
         BL    DISDEM4                                                          
         B     DISDEM2G                                                         
*                                                                               
***  2 DECIMAL  ***                                                             
DISDEM2E CLC   APFULL,=F'100000'   10X BIGGER THAN NORMAL                       
         BL    DISDEM4             SINCE IT'S 2 DECIMALS                        
         CLI   EBFLOAT,C'*'                                                     
         BE    *+14                                                             
         CLC   APFULL,=F'1000000'                                               
         BL    DISDEM4                                                          
         B     DISDEM2G                                                         
***  2 DECIMAL  ***                                                             
*                                                                               
***  2 DECIMAL  ***                                                             
DISDEM2G XR    RE,RE               USED TO BE JUST NO DECIMALS                  
         IC    RE,EBDECS                                                        
         BCTR  RE,0                DECREMENT WHETHER 1 OR 2 DECIMALS            
         STC   RE,EBDECS                                                        
***  2 DECIMAL  ***                                                             
***      MVI   EBDECS,0                                                         
         MVI   EBSCIN,X'81'                                                     
*                                                                               
DISDEM4  GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISDEM6  OI    6(R6),FVOXMT                                                     
         OI    FVIIND-FVIHDR(R6),FVIVAL   TURN ON PREV VALIDATED BIT            
*                                                                               
DISDEMX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALCULATE AN AUTO ADJUSTMENT PERCENTAGE                  *         
* INPUT  : LDEMOLD = OLD DEMO VALUE                                   *         
*          LDEMNEW = NEW DEMO VALUE                                   *         
* OUTPUT : LDEMADJ = PERCENT ADJUST FROM OLD TO NEW                   *         
***********************************************************************         
         SPACE 1                                                                
ADJCALC  DS    0H                                                               
***  2 DECIMAL                                                                  
         XC    APWORK+64(16),APWORK+64                                          
         MVC   APWORK+64(4),LDEMOLD   OLD TARGET RATING                         
         MVC   APWORK+68(4),LDEMNEW   NEW TARGET RATING                         
*                                                                               
         NI    APWORK+64,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         NI    APWORK+68,X'FF'-X'C0'   TAKE OFF 2D/OVERIDE BIT IF THERE         
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BZ    AD00K                - NOPE WE'RE NOT                            
***                                                                             
         TM    LDEMOLD,X'40'       2 DECIMAL?                                   
         BNZ   AD00E                - YUP, NOTHING TO DO                        
         L     R1,APWORK+64                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+64                                                     
AD00E    TM    LDEMNEW,X'40'       2 DECIMAL?                                   
         BNZ   AD00K                                                            
***  IT SEEMS LIKE LDEMNEW IS IN 2 DECIMAL WITHOUT X'40' BIT ON                 
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNZ   AD00K                - YUP                                       
***  IT SEEMS LIKE LDEMNEW IS IN 2 DECIMAL WITHOUT X'40' BIT ON                 
         L     R1,APWORK+68                                                     
         MHI   R1,10                                                            
         ST    R1,APWORK+68                                                     
*****  BOTH APFULL AND LNEWRTG SHOULD BE IN 2 DECIMAL MODE BY NOW               
AD00K    DS    0H                                                               
***  2 DECIMAL                                                                  
         L     RF,=F'1000'                                                      
         OC    APWORK+64(4),APWORK+64   TEST OLD DEMO VALUE = 0                 
         BNZ   AD1                                                              
         OC    APWORK+68(4),APWORK+68    YES-TEST NEW VALUE = 0                 
         BZ    AD2                 YES-ADJUST 100%                              
         B     AD99                NO-ERROR                                     
AD1      SR    RE,RE                                                            
         L     RF,APWORK+68                                                     
         M     RE,=F'2000'                                                      
         D     RE,APWORK+64                                                     
         AHI   RF,1                                                             
         SRA   RF,1                                                             
*                                                                               
AD2      ST    RF,LDEMADJ                                                       
         B     ADX                                                              
*                                                                               
AD99     MVC   FVMSGNO,=AL2(FVIDADJ)                                            
*                                                                               
ADX      CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY (WHEN IT IS SELECTED)                                             
***********************************************************************         
DISPKEY  MVC   IOKEY,APRECKEY      GET THE RECORD                               
*                                                                               
         NI    LINDS2,X'FF'-L2FRMDTL-L2XFRDTL                                   
         XC    NWSDTLKY,NWSDTLKY                                                
         CLI   IOKEY,NBRKTYPQ      NWS RECORD? (DETAIL OR REVISION)             
         BH    DISK00                                                           
         CLI   IOKEY+1,BWDKSUBQ    YES, DETAIL RECORD ?                         
         BNE   DISK00                                                           
         OI    LINDS2,L2FRMDTL                                                  
         MVC   NWSDTLKY,APRECKEY   SAVE THE NWS DETAIL KEY                      
         GOTO1 =A(DISPNDTL),RR=APRELO   YES, ONLY FROM WORK/SKED                
         B     DISKX                                                            
*                                                                               
DISK00   GOTO1 AIO,DIRHI+IO2                                                    
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
         MVC   REVMED,QMED         MEDIA                                        
*                                                                               
         LA    R1,BBYR                                                          
         ICM   R1,8,=X'01'                                                      
         GOTO1 AGETBYR                                                          
         MVC   REVBYR,QBYR         BUYER                                        
*                                                                               
         GOTO1 AGETCM,BCMSEQ       CAMPAIGN/MARKET                              
         MVC   REVNUM,QCAM                                                      
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
         CLI   APWORK+13,C' '                                                   
         BNE   *+16                                                             
         CLI   QMED,C'T'                                                        
         BNE   *+8                                                              
         MVI   APWORK+13,C'T'                                                   
*                                                                               
         MVC   REVSTA(5),APWORK+9  STATION                                      
         OI    REVSTAH+6,X'80'                                                  
         MVI   REVSTAH+5,5                                                      
         CLI   REVSTA+4,C' '                                                    
         BE    *+16                                                             
         CLI   REVSTA+4,C'T'                                                    
         BNE   DISK10                                                           
         MVI   REVSTA+4,C' '                                                    
         MVI   REVSTAH+5,4                                                      
*                                                                               
DISK10   CLI   REVSTA,C'0'                                                      
         BL    *+14                                                             
         MVI   REVSTA+4,C'/'                                                    
         MVC   REVSTA+5(3),APWORK+9+5                                           
*                                                                               
         CLI   0(R3),NBRKTYPQ                                                   
         BH    DISK20                                                           
         USING NBRKEY,R3                                                        
         ZIC   R1,NBRKNBSQ         SEQ NUMBER FOR MANUAL BUY REVISION           
         LA    R2,REVSEQH                                                       
         CLI   NBRKNBSQ,0                                                       
         BNE   DISK25                                                           
         LA    RE,NBRFSTEL                                                      
         USING NBRSELD,RE                                                       
         IC    R1,NBRSBYLN                                                      
         LA    R2,REVLINH                                                       
         DROP  RE                                                               
         B     DISK25                                                           
         USING BUYKEY,R3                                                        
DISK20   ZIC   R1,BUYKBUY          BUYLINE NUMBER FOR AGENCY BUY RECORD         
         LA    R2,REVLINH                                                       
*                                                                               
DISK25   CVD   R1,APDUB                                                         
         UNPK  8(3,R2),APDUB                                                    
         OI    8+2(R2),X'F0'                                                    
         MVI   5(R2),3                                                          
         OI    4(R2),X'08'         VALID NUMERIC                                
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DISKX    XIT1                                                                   
         EJECT                                                                  
BLANKS   DC    32C' '                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY NWS DETAIL RECORD SO THAT THE USER CAN ADD THE BUY REVISION           
***********************************************************************         
DISPNDTL NTR1  BASE=*,LABEL=*                                                   
         GOTO1 AMIN,MINRD2                                                      
         BNE   DNDTLX                                                           
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
*                                                                               
         GOTO1 AGETMED,BWDKAGMD                                                 
         BNE   *+10                                                             
         MVC   REVMED,QMED         MEDIA                                        
         LA    R1,BWDKBYR                                                       
         ICM   R1,8,=X'01'                                                      
         GOTO1 AGETBYR                                                          
         MVC   REVBYR,QBYR         BUYER                                        
         GOTO1 AGETCM,BWDKSEQ      CAMPAIGN/MARKET                              
         MVC   REVNUM,QCAM                                                      
         MVC   REVSTA(5),BWDSTA    STATION                                      
         CLI   REVSTA+4,C'T'                                                    
         BNE   *+8                                                              
         MVI   REVSTA+4,C' '                                                    
         CLI   REVSTA,C'0'                                                      
         BL    *+14                                                             
         MVI   REVSTA+4,C'/'                                                    
         MVC   REVSTA+5(3),BWDSTA+5                                             
*                                                                               
         XC    REVDAY,REVDAY       DAYS                                         
         CLI   BWDDAYS,0                                                        
         BE    DNDTL2                                                           
         GOTO1 AGETDAY,BWDDAYS                                                  
         MVC   REVDAY,QDAYS                                                     
*                                                                               
DNDTL2   XC    REVTIM,REVTIM       TIMES                                        
         OC    BWDTIMES,BWDTIMES                                                
         BZ    DNDTL3                                                           
         GOTO1 AGETTIM,BWDTIMES                                                 
         MVC   REVTIM,QTIMES                                                    
*                                                                               
DNDTL3   GOTO1 AGETDPT,BWDDPT      DAYPART/LENGTH                               
         MVC   REVDPL(1),BDPT                                                   
         LA    RF,REVDPL+1                                                      
         CLI   BDPT,C'0'           TEST NUMERIC DAYPART                         
         BL    *+12                                                             
         MVI   0(RF),C'/'          YES-SEPARATE DPT AND SLN WITH /              
         LA    RF,1(RF)                                                         
         ZIC   RE,BWDSLN                                                        
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APFULL(3),APDUB                                                  
         LA    RE,APFULL                                                        
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,APFULL+1                                                      
         MVI   2(RE),C' '                                                       
         MVC   0(3,RF),0(RE)                                                    
*                                                                               
         MVC   REVSDP,BWDSUBDP                                                  
*                                                                               
         TM    BWDINDS2,BWDINBR    ALREADY ADDED AS NBR?                        
         BZ    *+8                                                              
         OI    LINDS2,L2XFRDTL                                                  
*                                                                               
         TWAXC REVCS1H                                                          
         XC    EBLOCK,EBLOCK       SET UP EBLOCK FOR EDITING                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         LA    R1,REVCS1H          COST                                         
         LA    R8,BWDCOST1                                                      
         BAS   RE,DISCSTR                                                       
         OC    BWDEFDT2,BWDEFDT2   EFFECTIVE DATE 2                             
         BZ    DNDTLR1                                                          
         LA    R4,REVEF2H                                                       
         LA    R8,BWDEFDT2                                                      
         BAS   R9,DISDATR                                                       
         LA    R1,REVCS2H          COST 2                                       
         LA    R8,BWDCOST2                                                      
         BAS   RE,DISCSTR                                                       
         OC    BWDEFDT3,BWDEFDT3   EFFECTIVE DATE 3                             
         BZ    DNDTLR1                                                          
         LA    R4,REVEF3H                                                       
         LA    R8,BWDEFDT3                                                      
         BAS   R9,DISDATR                                                       
         LA    R1,REVCS3H          COST 3                                       
         LA    R8,BWDCOST3                                                      
         BAS   RE,DISCSTR                                                       
*                                                                               
*NDTLR1  MVC   REVREPN,BWDREP      ESTIMATE REP OR REP OVERRIDE                 
***  WE NEED TO UNPACK BWDREP  (SAVED IN BINARY)                                
DNDTLR1  OC    BWDREP,BWDREP       ANY BINARY REP VALUES?                       
         BZ    DNDTLR1C             - NOPE, USE LESTREP                         
***                                                                             
         GOTO1 VCOLY,APPARM,0,X'D9000ABC'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),APPARM,(C'U',BWDREP),REVREPN                                
***                                                                             
         OC    REVREPN,REVREPN     ANYTHING?                                    
         BNZ   DNDTLR1X             - YUP YUP                                   
DNDTLR1C MVC   REVREPN,LESTREP     LESTREP COULD BE EMPTY                       
DNDTLR1X OI    REVREPNH+6,FVOXMT                                                
*                                                                               
DNDTLR2  MVC   REVPRG,BWDPROG      PROGRAMMING                                  
         OI    REVPRGH+6,FVOXMT                                                 
*                                                                               
         XC    REVDAT,REVDAT       DATES                                        
         OI    REVDATH+6,FVOXMT                                                 
         OC    BWDDATES,BWDDATES                                                
         BZ    DNDTLR3                                                          
         GOTO1 VDATCON,APPARM,(2,BWDDATES),(8,REVDAT)                           
         MVI   REVDAT+8,C'-'                                                    
         GOTO1 (RF),(R1),(2,BWDDATES+2),(8,REVDAT+9)                            
*                                                                               
DNDTLR3  LA    R4,BWDEL                                                         
         XC    REVSK1,REVSK1       OVER TO THE HIDDEN COMMENT LINES             
         XC    REVSK2,REVSK2                                                    
         MVI   REVSK1H+5,0                                                      
         MVI   REVSK2H+5,0                                                      
         OI    REVSK1H+6,X'80'                                                  
         OI    REVSK2H+6,X'80'                                                  
*                                                                               
DNDTLR4  CLI   0(R4),0                                                          
         BE    DNDTLR40                                                         
         CLI   0(R4),BTRELCDQ      BUY TRANSFER ELEMENT?                        
         BE    *+12                                                             
         CLI   0(R4),DTRELCDQ        OR DAILY TRANSFER ELEMENT?                 
         BNE   *+12                                                             
         OI    LINDS2,L2XFRDTL     YES, CAN'T ADD BUY REVISION                  
         B     DNDTLR30                                                         
*                                                                               
         CLI   0(R4),BWIDECDQ      ID                                           
         BNE   DNDTLR5                                                          
         USING BWIDEL,R4                                                        
         MVC   REVID,BWID                                                       
         OI    REVIDH+6,FVOXMT                                                  
         B     DNDTLR30                                                         
*                                                                               
DNDTLR5  CLI   0(R4),UPGELCDQ      UPGRADE                                      
         BNE   DNDTLR6                                                          
         USING UPGEL,R4                                                         
         MVC   REVUPG,UPGINPUT                                                  
*                                                                               
         LA    RE,REVUPG+L'REVUPG-1  FIGURE OUT THE LENGTH                      
         LA    R0,REVUPG                                                        
DNDTLR5A CLI   0(RE),C' '                                                       
         BL    DNDTLR5L                                                         
         LA    RE,1(RE)                                                         
         SR    RE,R0                                                            
         STC   RE,REVUPGH+5                                                     
         B     DNDTLR5Z                                                         
*                                                                               
DNDTLR5L BCTR  RE,0                                                             
         CR    RE,R0                                                            
         BNL   DNDTLR5A                                                         
*                                                                               
DNDTLR5Z OI    REVUPGH+6,FVOXMT                                                 
         B     DNDTLR30                                                         
*                                                                               
DNDTLR6  CLI   0(R4),COMELCDQ      COMMENT                                      
         BNE   DNDTLR8                                                          
         USING COMEL,R4                                                         
         LA    RF,COMCOM-1                                                      
         ZIC   RE,COMELLN                                                       
         SH    RE,=Y(COMCOM-COMEL)                                              
         SR    R1,R1                                                            
         CLI   COMNUM,1            TEST OLD COMMENT ELEMENT                     
         BL    DNDTLR7             (WITHOUT NUMBER)                             
         CLI   COMNUM,5                                                         
         BH    DNDTLR7                                                          
         BNE   DNDTLR6A                                                         
         CLI   COMCOM,C'^'         SPECIAL SELLER COMMENT FROM TAM?             
         BE    DNDTLR30            YES, DON'T WANT IN BUYS NOR BUY REVS         
*                                                                               
DNDTLR6A BCTR  RE,0                                                             
         LA    RF,1(RF)                                                         
         IC    R1,COMNUM           COMMENT NUMBER                               
         BCTR  R1,0                                                             
         MH    R1,=Y(REVCM2-REVCM1)                                             
*                                                                               
DNDTLR7  LA    R1,REVCM1H(R1)                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),0(RF)                                                    
         OI    6(R1),FVOXMT                                                     
         B     DNDTLR30                                                         
*                                                                               
DNDTLR8  CLI   0(R4),DMOELCDQ      DEMOS                                        
         BNE   DNDTLR20                                                         
         USING DMOEL,R4                                                         
         ZIC   R9,DMOELLN                                                       
         LA    R9,DMOEL(R9)                                                     
         BCTR  R9,0                                                             
         LA    R8,L'DMODEMO                                                     
         LA    R1,DMODEMO                                                       
*                                                                               
DNDTLR10 LA    R0,LNDEMOS                                                       
         LA    R2,ESTDEMS                                                       
         LA    R6,REVDM1H                                                       
*                                                                               
DNDTLR12 CLC   1(2,R1),1(R2)                                                    
         BNE   DNDTLR14                                                         
         CLI   0(R1),OVERELEM                                                   
         BE    DNDTLR16                                                         
         CLC   0(1,R1),0(R2)                                                    
         BE    DNDTLR16                                                         
DNDTLR14 LA    R2,3(R2)                                                         
         BCT   R0,*+14                                                          
         MVC   FVMSGNO,=AL2(FVDEMES)                                            
         B     DNDTLX              ERROR - EST HDR DEMO CHANGED                 
*                                                                               
         ZIC   RF,0(R6)                                                         
         AR    R6,RF                                                            
         TM    FVATRB-FVIHDR(R6),FVAPROT                                        
         BO    *-10                                                             
         B     DNDTLR12                                                         
*                                                                               
DNDTLR16 MVC   APFULL,4(R1)                                                     
         GOTO1 ADISDEM                                                          
         BXLE  R1,R8,DNDTLR10                                                   
         B     DNDTLR30                                                         
*                                                                               
DNDTLR20 CLI   0(R4),SPWELCDQ      NEED TO COPY THE SCHEDULE                    
         BNE   DNDTLR27                                                         
         USING SPWEL,R4                                                         
         CLI   SPWELLN,SPWPERWK-SPWEL                                           
         BNH   DNDTLR30                                                         
         LA    RE,REVSK1                                                        
         LA    RF,SPWPERWK                                                      
         ZIC   R1,SPWELLN                                                       
         LA    R1,0(R1,R4)         BYTE AFTER THIS ELEMENT                      
*                                                                               
DNDTLR23 XR    R0,R0                                                            
         IC    R0,0(RF)                                                         
         CVD   R0,APDUB                                                         
         UNPK  0(2,RE),APDUB                                                    
         OI    1(RE),X'F0'                                                      
*                                                                               
         LA    RE,2(RE)            NEXT 2 BYTES FOR THIS WEEK                   
         LA    R0,REVSK2                                                        
         CR    RE,R0               ALREADY ON THE 2ND HIDDEN LINE?              
         BH    DNDTLR25            YES, WON'T HIT END OF THIS LINE              
*                                                                               
         LA    R0,REVSK1+L'REVSK1                                               
         CR    RE,R0                                                            
         BL    DNDTLR25            WE STILL HAVE ROOM ON THIS LINE              
         LA    R0,REVSK1                                                        
         SR    RE,R0                                                            
         STC   RE,REVSK1H+5                                                     
         OI    REVSK1H+6,X'80'                                                  
         LA    RE,REVSK2           CONTINUE ON THE NEXT HIDDEN LINE             
*                                                                               
DNDTLR25 LA    RF,1(RF)            NEXT WEEK IN SPWPERWK                        
         CR    RF,R1                                                            
         BL    DNDTLR23                                                         
*                                                                               
         LA    R0,REVSK2                                                        
         LA    R1,REVSK2H                                                       
         CR    RE,R0               ALREADY ON THE 2ND HIDDEN LINE?              
         BNL   *+12                YES                                          
         LA    R0,REVSK1                                                        
         LA    R1,REVSK1H                                                       
*                                                                               
         SR    RE,R0                                                            
         STC   RE,5(R1)                                                         
         OI    6(R1),X'80'                                                      
*                                                                               
DNDTLR27 CLI   0(R4),CS2ELCDQ      COST 2 OVERRIDE ELEMENT                      
         BNE   DNDTLR30                                                         
         USING CS2EL,R4                                                         
         OI    REVC2NH+6,FVOXMT                                                 
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVI   EBALIGN,C'L'                                                     
         LA    R1,REVC2NH                                                       
         LA    R8,CS2COST2                                                      
         BAS   RE,DISCSTR                                                       
*                                                                               
DNDTLR30 XR    R0,R0               NEXT ELEMENT                                 
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DNDTLR4                                                          
*                                                                               
DNDTLR40 DS    0H                                                               
*                                                                               
DNDTL99  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
DNDTLX   XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY COST ROUTINE                                                *         
* R1 = A(FIELD HEADER)                                                *         
* R8 = A(4-BYTE COST)                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCSTR  NTR1                                                                   
         LR    R2,R1                                                            
         MVI   EBDECS,2                                                         
         ST    R8,EBAIN                                                         
         LA    RE,L'FVIHDR(R2)                                                  
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,L'REVCS1                                                  
         ICM   R4,15,0(R8)                                                      
         BNZ   *+14                                                             
         MVC   0(2,RE),=C'$0'                                                   
         B     DISCSTR7                                                         
         SR    RE,RE                                                            
         LR    RF,R4                                                            
         D     RE,=F'100'                                                       
         LTR   RE,RE                                                            
         BZ    *+12                                                             
         C     RF,=F'100000'                                                    
         BL    DISCSTR2                                                         
         MVI   EBDECS,0                                                         
         ST    RF,APDUB                                                         
         LA    RE,APDUB                                                         
         ST    RE,EBAIN                                                         
*                                                                               
DISCSTR2 GOTO1 VEDITOR,APPARM,EBLOCK                                            
*                                                                               
DISCSTR7 OI    6(R2),FVOXMT                                                     
         OI    FVIIND-FVIHDR(R2),FVIVAL  SET PREVIOUSLY VALIDATED BIT           
*                                                                               
         LA    RE,8(R2)            FIGURE L(COST FIELD)                         
         XR    RF,RF                                                            
DISCSTR8 CLI   0(RE),C' '                                                       
         BNH   DISCSTR9                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    R1,8+L'REVCS1(R2)                                                
         CR    RE,R1                                                            
         BNH   DISCSTR8                                                         
DISCSTR9 STC   RF,5(R2)                                                         
DISCSTRX B     DNDTLX                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATE ROUTINE                                                *         
* R4 = A(FIELD HEADER)                                                *         
* R8 = A(3-BYTE DATE)                                                 *         
* R9 = RETURN ADDRESS                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISDATR  GOTO1 VDATCON,APPARM,(3,0(R8)),(8,L'FVIHDR(R4))                        
         OI    6(R4),FVOXMT                                                     
         MVI   5(R4),8                                                          
         BR    R9                                                               
         LTORG                                                                  
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
AXTRA    DS    0F               ** EXTENTION ROUTINE ADDRESSES **               
AVALDATE DS    A                                                                
AVALRDAY DS    A                                                                
ACHPTDOL DS    A                                                                
AGETDEMS DS    A                                                                
ADISDEM  DS    A                                                                
AADJCALC DS    A                                                                
ADISPKEY DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
LASAVE   DS    A                                                                
LHDRDA   DS    F                                                                
LDEMVALS DS    (LNDEMOS)XL4                                                     
LDEMOLD  DS    F                                                                
LDEMNEW  DS    F                                                                
LDEMADJ  DS    F                                                                
LADJUST  DS    F                                                                
LRTG     DS    F                                                                
LSVREG1  DS    F                                                                
*                                                                               
BBUYLINE DS    XL1                 BINARY LINE NUMBER                           
BSEQNUM  DS    XL1                 BINARY SEQ NUMBER                            
*BSEQNUM2 IS FARTHER DOWN                                                       
*                                                                               
LINDS    DS    XL1                                                              
LNOHDR   EQU   X'80'                                                            
LAUTADJ  EQU   X'40'                                                            
LALLADJ  EQU   X'20'                                                            
LNEWSTA  EQU   X'10'                                                            
LPKGSLV  EQU   X'08'                                                            
LCHG     EQU   X'04'                                                            
LNOTPOCM EQU   X'02'               NOT PART OF THIS CAMPAIGN                    
LADDREC  EQU   X'01'               NEED TO ADD RECORD                           
*                                                                               
LINDS2   DS    XL1                                                              
L2RECDEL EQU   X'80'               RECORD IS DELETED                            
L2PRGCHG EQU   X'40'               PROGRAM NAME CHANGED                         
L2FRMDTL EQU   X'20'               FROM A DETAIL RECORD                         
L2XFRDTL EQU   X'10'               DTL COPIED OR TRANSFERRED ALREADY            
L2FRMWRK EQU   X'08'               THIS CAME FROM WORK SKED!!  C'>' OPT         
L2DTUCHG EQU   X'04'               DAY, TIME, OR UPGRADE CHANGED                
L2COS2   EQU   X'02'                - WE HAVE A COS2 FACTOR                     
*                                                                               
NWSDTLKY DS    XL(L'APRECKEY)      SAVED NWS DTL KEY                            
*                                                                               
LHDRKEY  DS    XL32                                                             
LTMPKEY  DS    XL32                                                             
LSEQNUM  DS    XL1                                                              
LSUBDPT  DS    CL1                                                              
LNDEMOS  EQU   14                                                               
LDNAMES  DS    (LNDEMOS)CL7                                                     
LDEMS    DS    (LNDEMOS)XL3                                                     
         DS    X                                                                
LUPFIL   DS    C                                                                
LUPGRD   DS    XL8                                                              
LUPFRBK  DS    XL2                                                              
LUPFRBKT DS    XL1                 LUPFRBK BOOK TYPE                            
LUPFRBKL DS    XL6                                                              
LUPPUT   DS    CL1                                                              
LUPSHR   DS    CL1                                                              
LOVDAY   DS    X                                                                
LOVTIME  DS    XL4                                                              
LDEMOVR  DS    (LNDEMOS)XL6                                                     
         DS    X                                                                
LDMUPBLK DS    (SPDEMUP2)X                                                      
LSPWEL   DS    XL64                                                             
LBTREL   DS    XL300               <===  EXPANDED FROM 256 TO 300               
LBTREL2  DS    XL300               <===  EXPANDED FROM 256 TO 300               
LBTREL3  DS    XL300               <===  EXPANDED FROM 256 TO 300               
LDTREL   DS    XL256                                                            
LDMOEL   DS    XL128                                                            
LBWDELSV DS    XL(BWDELLNQ)                                                     
LSEQNOS  DS    XL256                                                            
LIMPVALS DS    (LNDEMOS)XL6                                                     
LALLVALS DS    (LNDEMOS)XL6                                                     
LADJDEM  DS    XL2                                                              
LESTREP  DS    CL3                 SAVE THE ESTIMATE DEFAULT REP CODE           
LREP     DS    XL2                 SAVE THE REP CODE                            
LID      DS    CL12                                                             
LTRT     DS    XL256                                                            
*                                                                               
BSEQNUM2 DS    XL1                 SAVE BINARY SEQ NUMBER, FOR DUPING           
         EJECT                                                                  
* DEDBLOCK                                                                      
         DS    0H                                                               
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDEBLOCK                                                                      
         DS    0H                                                               
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
         SPACE 3                                                                
LIUN     DS    1000X                                                            
         SPACE 1                                                                
LOCALX   EQU   *                                                                
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSD7D                                                       
         EJECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSECD                                                       
         EJECT                                                                  
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSBRV                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSBRV                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPNWSCOMP                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPNWSCOMP                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENEST                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         SPACE 1                                                                
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         SPACE 1                                                                
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135SPNWS36   08/25/08'                                      
         END                                                                    
