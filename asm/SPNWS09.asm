*          DATA SET SPNWS09    AT LEVEL 057 AS OF 02/26/07                      
*PHASE T20709C,*                                                                
         TITLE 'BWS09 - BUYERS WORK SHEET - CANADIAN SPILL'                     
T20709   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20709**,RA,RR=RE                                              
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(SAVE AREA)                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         L     RC,APALOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
*                                                                               
         LH    R1,=Y(SAVAREAX-SAVAREA)                                          
         LA    R1,0(R1,R6)                                                      
         ST    R1,LADEMTAB         SET A(DEMO TABLE). MAX=8K?? DOUBT IT         
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         DC    AL4(0)  VALKEY                                                   
         DC    AL4(0)  VALREC                                                   
         DC    AL4(0)  DISKEY                                                   
         DC    AL4(0)  DISREC                                                   
         DC    AL4(0)  DELREC                                                   
         DC    AL4(0)  RESREC                                                   
         B     VALPAR                                                           
         DC    AL4(0)  GETSEL                                                   
         DC    AL4(0)  DISSEL                                                   
         DC    AL4(0)  VALSEL                                                   
         B     EXIT    FSTLST                                                   
         DC    AL4(0)                                                           
         DC    AL4(0)  FSTSCR                                                   
         DC    AL4(0)  LASSCR                                                   
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
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT PARAMETERS                                          *         
* THE CAMPAIGN'S SPILL DEMOS ARE CALCULATED AND SPILL ELEMENTS ARE    *         
* ADDED TO THE CAMPAIGN/MARKET HEADER RECORDS FOR ALL SPILL MARKETS.  *         
* TSAR NET/SPILL RECORDS ARE ADDED OR UPDATED WITH THIS LATEST SPILL  *         
* INFORMATION.                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALPAR   GOTO1 AVALPARM,BWSKY2H    VALIDATE SELECT PARAMETERS                   
         BNE   VALPX                                                            
         TM    TWAFLAG,TWANODET    TEST FOR NO DETAIL RECORDS                   
         BNZ   VALPX                                                            
*                                                                               
         BAS   RE,GETMKTS          GET THE SPILL MARKETS                        
         BNE   VALPX                                                            
*                                                                               
         L     R1,LADEMTAB         CLEAR TABLE OF DEMO TOTALS                   
         LA    R0,MAXENTS                                                       
         XC    0(DRECL,R1),0(R1)                                                
         LA    R1,DRECL(R1)                                                     
         BCT   R0,*-10                                                          
         XC    LNENTS,LNENTS                                                    
*                                                                               
         LA    R8,LDMUPBLK         INITIALIZE SPDEMUP BLOCK                     
         USING SPDEMUPD,R8                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         L     RE,ATIA                                                          
         ST    RE,SPUPAREC         (USE TIA FOR IUN RECORD)                     
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
         MVC   SPUPSRC,CLTSRC                                                   
         TM    CLTIND2,CLTIANFR    TEST 1W ANGLO/FRANCO OPTION ON               
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
         MVC   LDEM(3),ESTDEMS     TARGET DEMO                                  
         MVI   LDEM+3,X'FF'                                                     
*                                                                               
         L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         LA    R3,IOKEY                                                         
         USING BWDRECD,R3                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(13),APRECKEY                                               
         LA    R4,BWDKELST-BWDKEY  SET R4 FOR KEY COMPARE                       
         CLI   BWDKELST,0          TEST STATION FILTER                          
         BE    *+8                                                              
         LA    R4,L'BWDKELST(R4)   YES                                          
         BCTR  R4,0                                                             
         LA    R1,MINHI2                                                        
         B     VALP2+4                                                          
*                                                                               
VALP2    LA    R1,MINSEQ2          READ ALL DETAIL RECORDS                      
         GOTO1 AMIN                                                             
         BNE   VALP22                                                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   VALP22                                                           
         L     R3,AIOAREA2                                                      
         OC    QSTA,QSTA           TEST STATION FILTER                          
         BZ    *+16                                                             
         CLC   QSTA,BWDSTA         YES-CHECK STATION IS CORRECT                 
         BE    *+6                                                              
         DC    H'0'                NO-EUGH                                      
         CLI   BDPT,0              TEST DAYPART FILTER                          
         BE    VALP4                                                            
         CLC   BWDDPT,BDPT         YES-MATCH DPT OR SUBDPT                      
         BE    VALP4                                                            
         CLC   BWDSUBDP,BDPT                                                    
         BNE   VALP2                                                            
*                                                                               
VALP4    CLI   BSLN,0              TEST LENGTH FILTER                           
         BE    *+14                                                             
         CLC   BWDSLN,BSLN         YES-MATCH SPOT LENGTH                        
         BNE   VALP2                                                            
         XC    APDUB,APDUB                                                      
         XC    LASPWEL,LASPWEL                                                  
         XC    LASPIEL,LASPIEL                                                  
         LA    RF,BWDEL            FIND UPGRADE ELEMENTS (IF ANY)               
         SR    R0,R0                                                            
*                                                                               
VALP6    CLI   0(RF),0                                                          
         BE    VALP8                                                            
         CLI   0(RF),UPGELCDQ                                                   
         BNE   *+12                                                             
         ST    RF,APDUB            SAVE A(UPGRADE ELEMENT)                      
         B     VALP7                                                            
         CLI   0(RF),ODTELCDQ                                                   
         BNE   *+12                                                             
         ST    RF,APDUB+4          SAVE A(OVERRIDE DAY/TIME/STA ELEM)           
         B     VALP7                                                            
         CLI   0(RF),SPWELCDQ                                                   
         BNE   *+12                                                             
         ST    RF,LASPWEL          SAVE A(SPOTS PER WEEK ELEMENT)               
         B     VALP7                                                            
         CLI   0(RF),SPIELCDQ                                                   
         BNE   VALP7                                                            
         OC    LASPIEL,LASPIEL                                                  
         BNZ   VALP7                                                            
         ST    RF,LASPIEL          SAVE A(FIRST SPILL ELEMENT)                  
*                                                                               
VALP7    IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     VALP6                                                            
*                                                                               
VALP8    OC    LASPWEL,LASPWEL     TEST FOR SPOTS/WEEK ELEMENT                  
         BZ    VALP2               NO-IGNORE RECORD                             
*                                                                               
         XC    SPUPSTA,SPUPSTA     CLEAR UPGRADE VALUES                         
         MVI   SPUPDAY,0                                                        
         XC    SPUPTIM,SPUPTIM                                                  
         MVI   SPUPFIL,0                                                        
         XC    SPUPFBK,SPUPFBK                                                  
         XC    SPUPFBKL,SPUPFBKL                                                
         XC    SPUPTYPE(8),SPUPTYPE                                             
         MVI   SPUPUDAY,0                                                       
         XC    SPUPUTIM,SPUPUTIM                                                
         MVI   LUPPUT,0                                                         
         MVI   LUPSHR,0                                                         
         ICM   R9,15,APDUB         TEST BWS DETAIL UPGRADE                      
         BZ    VALP10                                                           
         USING UPGEL,R9            YES -                                        
         MVC   SPUPFIL,UPGFILE     UPGRADE FILE                                 
         MVC   SPUPFBK,UPGFRBK     OVERRIDE SHARE BOOK                          
         CLI   UPGELLN,51                                                       
         BL    *+10                                                             
         MVC   SPUPFBKL,UPGFRBKL   SHARE BOOK LIST                              
         MVC   SPUPTYPE(8),UPGRADE UPGRADE EXPRESSION                           
         MVC   LUPPUT,BWDUPUT      PUT AVERAGING                                
         MVC   LUPSHR,BWDUSHR      SHR AVERAGING                                
         B     VALP12                                                           
*                                                                               
VALP10   OC    CMPUP,CMPUP         NO - TEST DEFAULT CAMPAIGN UPGRADE           
         BZ    VALP12                                                           
         MVC   SPUPFIL,CMPUF            YES                                     
         MVC   SPUPFBK,CMPFB                                                    
         MVC   SPUPFBKL,CMPFBLST                                                
         MVC   SPUPTYPE(8),CMPUP                                                
         MVC   LUPPUT,CMPUPUT                                                   
         MVC   LUPSHR,CMPUSHR                                                   
*                                                                               
VALP12   ICM   R9,15,APDUB+4                                                    
         BZ    VALP14                                                           
         USING ODTEL,R9                                                         
         MVC   SPUPSTA,ODTSTA      OVERRIDE STATION                             
         MVC   SPUPUDAY,ODTDAY     OVERRIDE DAY                                 
         MVC   SPUPUTIM,ODTTIME    OVERRIDE TIME                                
*                                                                               
VALP14   OC    SPUPTYPE(8),SPUPTYPE   TEST ANY UPGRADE VALUES                   
         BZ    VALP99                 NO-ERROR                                  
         CLI   LUPPUT,C'1'         FIX THE PUT/SHR AVERAGING VALUES             
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
         OC    SPUPSTA,SPUPSTA     REST OF SPDEMUP BLOCK                        
         BNZ   VALP14G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   BWDSTA,C'0'         IS IT A NUMBER?                              
         BL    VALP14E              - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),BWDSTA+5   MOVE THE NETWORK IN                        
         MVC   SPUPSYSE,BWDSTA                                                  
         B     VALP14G                                                          
*****  CABLE/FUSION DATE LOOKUP                                                 
VALP14E  MVC   SPUPSTA,BWDSTA                                                   
VALP14G  MVC   SPUPDAY,BWDDAYS                                                  
         CLI   BWDDAYS,0           TEST PACKAGE/ORBIT                           
         BNE   *+10                                                             
         MVC   SPUPDAY,BWDPODAY                                                 
         MVC   SPUPTIM,BWDTIMES                                                 
         OC    SPUPFBK,SPUPFBK                                                  
         BNZ   *+16                                                             
         MVC   SPUPFBK,BWDBOOK                                                  
         XC    SPUPFBKL,SPUPFBKL                                                
*                                                                               
         LA    R9,LMKTS                                                         
         LA    R0,MAXSTAS          FIND LIST OF SPILL MARKETS                   
*                                  FOR THIS STATION                             
VALP16   OC    0(4,R9),0(R9)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   0(4,R9),BWDSTA                                                   
         BE    *+14                                                             
         LA    R9,L'LMKTS(R9)                                                   
         BCT   R0,VALP16                                                        
         DC    H'0'                                                             
         LA    R9,4(R9)                                                         
         LA    R0,MAXMKTS                                                       
*                                                                               
VALP18   OC    0(4,R9),0(R9)       CALL SPDEMUP FOR EACH SPILL MKT              
         BZ    VALP20                                                           
         OC    LASPIEL,LASPIEL     TEST SPILL DEMO ELEMENT(S)                   
         BZ    *+12                                                             
         BAS   RE,GETOVR           YES-GET OVERRIDE (IF ANY)                    
         BE    VALP19                                                           
         MVC   SPUPSPL,2(R9)       RATING SERVICE SPILL MKT                     
         MVC   SPUPBTYP,4(R9)      SPECIAL BOOK TYPE                            
*                                                                               
         CLI   QBOOKTYP,0                                                       
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
*                                                                               
         TM    SPUPFBK+1,BTYBITSQ  SPECIAL BOOK?                                
         BZ    VALP18R                                                          
         TM    SPUPFBK+1,BTY2CHAR  2 CHARACTER BOOKTYPE?                        
         BNO   VALP18O              - NOPE                                      
         MVC   SPUPBTYP,CMPFBTP     - YUP                                       
         B     VALP18Q                                                          
*                                                                               
VALP18O  GOTO1 AGETBKTY,APPARM,(C'B',SPUPFBK+1),SPUPBTYP                        
VALP18Q  NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
*                                                                               
VALP18R  XC    LDEMVAL,LDEMVAL                                                  
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   VALP18T                                                          
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
VALP18T  GOTO1 ASPDEMUP,APPARM,LDMUPBLK,LDEM,LDEMVAL                            
*                                                                               
VALP19   OC    LDEMVAL,LDEMVAL                                                  
         BZ    *+8                                                              
         BAS   RE,DEMADD           ADD TO DEMO TABLE                            
         LA    R9,5(R9)                                                         
         BCT   R0,VALP18           NEXT SPILL MARKET                            
*                                                                               
VALP20   B     VALP2               READ NEXT RECORD                             
*                                                                               
VALP22   L     R9,LADEMTAB         SORT THE DEMO TABLE                          
         ICM   R6,15,LNENTS                                                     
         BZ    VALP24                                                           
         LA    R1,DRECL                                                         
         ST    R1,APPARM+8                                                      
         LA    R1,L'DKEY                                                        
         ST    R1,APPARM+12                                                     
         GOTO1 VXSORT,APPARM,(R9),(R6),,,0                                      
*                                                                               
VALP24   BAS   RE,ADDSPILL         ADD SPILL TO CAMPAIGN/MARKET HDRS            
         BNE   VALPX                                                            
         OC    LNENTS,LNENTS                                                    
         BZ    VALP98                                                           
*                                                                               
VALP90   MVC   FVMSGNO,=AL2(FVSPILL)                                            
         MVI   FVOMTYP,C'I'                                                     
         LA    R1,FVXTRA                                                        
         MVC   FVXTRA,SPACES       OUTPUT SPILL MARKETS TO MESSAGE              
         L     R2,LADEMTAB                                                      
         USING DRECD,R2                                                         
         LA    R0,MAXENTS                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
VALP92   ICM   RE,3,DMKT                                                        
         BZ    VALP96                                                           
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         CR    RE,RF                                                            
         BE    VALP94                                                           
         LR    RF,RE                                                            
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(4,R1),APDUB                                                    
         MVI   4(R1),C','                                                       
         LA    R1,6(R1)                                                         
*                                                                               
VALP94   LA    R2,DRECL(R2)                                                     
         BCT   R0,VALP92                                                        
*                                                                               
VALP96   BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   VALPX                                                            
         MVI   0(R1),C' '                                                       
         B     VALPX                                                            
*                                                                               
VALP98   MVC   FVMSGNO,=AL2(FVNOSPDM)    NO SPILL DEMOS                         
         B     VALPX                                                            
*                                                                               
VALP99   MVC   FVMSGNO,=AL2(FVNOCNUP)    UPGRADE MISSING                        
*                                                                               
VALPX    MVI   BCAM,0              OTHERWISE IT THINKS CMPDATSD ARE SET         
         B     EXIT                                    CMPDATSP                 
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT SPILL OVERRIDES                                  *         
* INPUT  : R9=AAGENCY MARKET (2) / RATING SERVICE MARKET (2)|        *         
***********************************************************************         
         SPACE 1                                                                
GETOVR   NTR1  ,                                                                
         ICM   RE,15,LASPIEL                                                    
         BZ    OVRNE                                                            
         USING SPIEL,RE                                                         
         SR    RF,RF                                                            
*                                                                               
OVR2     CLC   SPIRMKT,2(R9)       YES-FIND ELEMENT FOR THIS MKT                
         BE    OVR6                                                             
*                                                                               
OVR4     IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    OVRNE                                                            
         CLI   0(RE),SPIELCDQ                                                   
         BNE   OVR4                                                             
         B     OVR2                                                             
*                                                                               
OVR6     ZIC   RF,1(RE)            FOUND-LOOK FOR DEMO OVERRIDE                 
         AR    RF,RE                                                            
         BCTR  RF,0                                                             
         LA    R1,SPIDEMO                                                       
         USING SPIDEMO,R1                                                       
         DROP  RE                                                               
         LA    RE,L'SPIDEMO                                                     
*                                                                               
OVR8     CLC   SPIDEMO+1(2),LDEM+1                                              
         BNE   OVR10                                                            
         TM    SPIDEMO+4,SPIDEMOV     TEST FOR OVERRIDE                         
         BZ    OVRNE                                                            
         MVC   LDEMVAL,SPIDEMO+4      YES-GRAB VALUE FROM ELEMENT               
         NI    LDEMVAL,255-SPIDEMOV       TURN OFF OVERRIDE BIT                 
         B     OVREQ                                                            
*                                                                               
OVR10    BXLE  R1,RE,OVR8                                                       
*                                                                               
OVRNE    LTR   RE,R0               CC NE - OVERRIDE NOT FOUND                   
         B     OVRX                                                             
*                                                                               
OVREQ    LR    RE,R0               CC EQ - OVERRIDE FOUND                       
         CR    RE,RE                       AND RETURNED IN LDEMVAL              
*                                                                               
OVRX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD SPILL DEMO VALUE TO TABLE OF DEMO TOTALS                        *         
* INPUT  : R3=A(BWS DETAIL RECORD)                                    *         
*          R9=A(MARKET CODE)                                          *         
*          LDEMVAL=SPILL DEMO VALUE                                   *         
*          LASPWEL=A(SPOTS PER WEEK ELEMENT)                          *         
***********************************************************************         
         SPACE 1                                                                
DEMADD   NTR1  ,                                                                
         LA    R2,APWORK                                                        
         USING DRECD,R2                                                         
         XC    DKEY,DKEY           BUILD DEMO TABLE KEY                         
         MVC   DMKT,0(R9)                                                       
         MVC   DDPT,BDPT                                                        
         CLI   BDPT,0                                                           
         BNE   *+10                                                             
         MVC   DDPT,BWDDPT                                                      
         MVC   DSLN,BWDSLN                                                      
         MVC   DSTA,BWDSTA                                                      
         L     R2,LADEMTAB         FIND ENTRY                                   
         LA    R0,MAXENTS                                                       
         LA    RE,1                                                             
*                                                                               
DEM1     OC    DKEY,DKEY                                                        
         BZ    *+18                                                             
         CLC   DKEY,APWORK                                                      
         BE    DEM3                                                             
         B     DEM2                                                             
         MVC   DKEY,APWORK                                                      
         ST    RE,LNENTS           UPDATE N'ENTRIES IN TABLE                    
         B     DEM3                                                             
*                                                                               
DEM2     LA    R2,DRECL(R2)                                                     
         LA    RE,1(RE)                                                         
         BCT   R0,DEM1                                                          
         DC    H'0'                INCREASE MAXENTS                             
*                                                                               
DEM3     L     R8,LASPWEL                                                       
         USING SPWEL,R8                                                         
         ZIC   R9,SPWELLN          SPOTS PER WEEK ELEMENT                       
         AR    R9,R8                                                            
         BCTR  R9,0                                                             
         LA    R6,SPWPERWK                                                      
         LA    R8,1                                                             
         LA    R4,DDEMS                                                         
         ZIC   R0,CMPNWKS                                                       
*                                                                               
DEM4     SR    RF,RF                                                            
         ICM   RF,1,0(R6)          RF=N'SPOTS SCHEDULED THIS WEEK               
         BZ    DEM6                                                             
         M     RE,LDEMVAL          SPOTS X DEMO VALUE                           
         L     RE,0(R4)            ACCUMULATE TOTAL DEMO VALUE                  
         AR    RE,RF               FOR THIS WEEK                                
         ST    RE,0(R4)                                                         
*                                                                               
DEM6     LA    R4,4(R4)                                                         
         BCT   R0,*+8                                                           
         B     DEMX                                                             
         BXLE  R6,R8,DEM4                                                       
*                                                                               
DEMX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD SPILL DEMOS TO CAMPAIGN/MARKET HEADER RECORDS                   *         
***********************************************************************         
         SPACE 1                                                                
ADDSPILL NTR1  ,                                                                
         LA    R2,IOKEY            BUILD 1ST PART OF HEADER KEY                 
         USING BWHRECD,R2                                                       
         XC    BWHKEY,BWHKEY                                                    
         MVI   BWHKTYP,BWHKTYPQ                                                 
         MVI   BWHKSUB,BWHKSUBQ                                                 
         MVC   BWHKAGMD,BAGYMD                                                  
         OC    BWHKAGMD,BBYRMASK                                                
         MVC   BWHKBYR,BBYR                                                     
         MVC   BWHKCAM,BCAM                                                     
*                                                                               
         ICM   R0,15,LNENTS                                                     
         BZ    ADDS8                                                            
         L     R4,LADEMTAB         LOOP THROUGH DEMOS TABLE                     
         USING DRECD,R4                                                         
*                                                                               
ADDS2    CLC   DMKT,BWHKMKT        TEST NEW SPILL MARKET                        
         BE    ADDS6                                                            
         OC    BWHKMKT,BWHKMKT     AND NOT FIRST TIME                           
         BZ    *+8                                                              
         BAS   RE,WRTHDR           YES-WRITE OFF PREVIOUS HEADER RECORD         
         MVC   BWHKMKT,DMKT        READ HEADER FOR THIS MARKET                  
         MVI   LNEWHDR,C'N'                                                     
         GOTO1 AIO,DIRHI+IO3                                                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ADDS3                                                            
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BNE   ADDS3                                                            
         GOTO1 AIO,FILGETU3        GET THE RECORD                               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DELELS           DELETE ALL RELEVENT SPILL ELEMENTS           
         B     ADDS6                                                            
*                                                                               
ADDS3    MVI   LNEWHDR,C'Y'        NOT FOUND-                                   
         MVC   APWORK(13),IOKEYSAV                                              
         XC    IOKEY,IOKEY         READ PASSIVE POINTERS TO GET                 
         MVI   BWHPTYP,BWHPTYPQ    NEXT CAMPAIGN/MARKET SEQ NO                  
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,BAGYMD                                                  
         OC    BWHPAGMD,BBYRMASK                                                
         MVC   BWHPBYR,BBYR                                                     
         GOTO1 AIO,DIRHID                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   APHALF,EFFS                                                      
         CLC   IOKEY(BWHPSEQ-BWHPKEY),IOKEYSAV  TEST FIRST CAMPAIGN             
         BNE   ADDS4                            FOR BUYER                       
         SR    RE,RE               NO-NEXT SEQ NO                               
         ICM   RE,3,BWHPSEQ                                                     
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    ADDS99                                                           
         STCM  RE,3,APHALF                                                      
*                                                                               
ADDS4    MVC   BWHPSEQ,APHALF      CAMPAIGN/MARKET SEQ NUM                      
         MVC   LPSVKEY,IOKEY       SAVE PASSIVE KEY                             
         L     R2,AIOAREA3                                                      
         XC    0(256,R2),0(R2)     BUILD HEADER RECORD                          
         MVC   BWHKEY,APWORK                                                    
         MVC   BWHKSEQ,APHALF                                                   
         MVC   IOKEY(13),BWHKEY                                                 
         MVI   BWHELCD,BWHELCDQ    DESCRIPTION ELEMENT                          
         MVI   BWHELLN,BWHELLNQ                                                 
         LA    R1,BWHFSTEL-BWHKEY                                               
         STCM  R1,3,BWHLEN                                                      
*                                                                               
ADDS6    XC    APELEM,APELEM       BUILD SPILL ELEMENT                          
         LA    R8,APELEM                                                        
         USING SPLELD,R8                                                        
         MVI   SPLELCD,SPLELCDQ                                                 
         MVC   SPLDPT,DDPT                                                      
         MVC   SPLSLN,DSLN                                                      
         MVC   SPLSTA(L'DSTA),DSTA                                              
         ZIC   RE,CMPNWKS                                                       
         SLL   RE,2                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   SPLDEMS(0),DDEMS                                                 
         LA    RE,SPLDEMS-SPLELD+1(RE)                                          
         STC   RE,SPLELLN          ELEMENT LENGTH                               
         L     R2,AIOAREA3                                                      
         GOTO1 AADDELS,BWHRECD     ADD NEW SPILL ELEMENT                        
*                                                                               
         LA    R2,IOKEY                                                         
         LA    R4,DRECL(R4)        NEXT DEMO TABLE ENTRY                        
         BCT   R0,ADDS2                                                         
*                                                                               
         BAS   RE,WRTHDR           WRITE OFF LAST HEADER RECORD                 
*                                                                               
ADDS8    LA    R8,SPLMKTS          LOOP THROUGH LIST OF SPILL MARKETS           
         LA    R0,L'SPLMKTS/2      TO FIND THOSE WHICH AREN'T SCHEDULED         
*                                                                               
ADDS10   OC    0(2,R8),0(R8)                                                    
         BZ    ADDSX                                                            
         L     R4,LADEMTAB                                                      
         ICM   RF,15,LNENTS                                                     
         BZ    ADDS12              NOTHING SCHEDULED NOW                        
         CLC   DMKT,0(R8)                                                       
         BE    ADDS14                                                           
         LA    R4,DRECL(R4)                                                     
         BCT   RF,*-14                                                          
*                                                                               
ADDS12   LA    R2,IOKEY            READ HEADER FOR SPILL MARKET                 
         MVC   BWHKMKT,0(R8)                                                    
         GOTO1 AIO,DIRHI+IO3                                                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     *+14                                                             
         CLC   IOKEY(BWHKSEQ-BWHKEY),IOKEYSAV                                   
         BE    *+14                                                             
         MVC   IOKEY,IOKEYSAV      HEADER NOT FOUND                             
         B     ADDS14                                                           
         GOTO1 AIO,FILGETU3        HEADER FOUND --                              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DELELS           DELETE RELEVENT SPILL ELEMENTS               
         MVI   LNEWHDR,C'N'                                                     
         BAS   RE,WRTHDR           WRITE BACK HEADER AND UPDATE TSAR            
*                                                                               
ADDS14   LA    R8,2(R8)            NEXT MARKET                                  
         BCT   R0,ADDS10                                                        
         B     ADDSX                                                            
*                                                                               
ADDS99   MVC   FVMSGNO,=AL2(FVCMSEQ)   OUT OF SEQUENCE NUMBERS                  
         OI    FVERRIND,FVEUNWND                                                
*                                                                               
ADDSX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE CAMPAIGN/MARKET HEADER RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
WRTHDR   NTR1  ,                                                                
         LA    R1,FILADD3          ADD                                          
         CLI   LNEWHDR,C'Y'                                                     
         BE    *+8                                                              
         LA    R1,FILPUT3          OR PUTREC                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LNEWHDR,C'Y'        TEST ADD                                     
         BNE   WRTHDR2                                                          
         XC    IOKEY,IOKEY         YES-ALSO ADD PASSIVE POINTER                 
         MVC   IOKEY(13),LPSVKEY                                                
         LA    R2,IOKEY                                                         
         USING BWHRECD,R2                                                       
         MVC   BWHKCNTL+1(4),IODA                                               
         GOTO1 AIO,DIRADD                                                       
         BE    WRTHDR2                                                          
         DC    H'0'                                                             
*                                                                               
WRTHDR2  XC    IOKEY,IOKEY         RESTORE KEY UP TO MARKET                     
         L     R2,AIOAREA3                                                      
         MVC   IOKEY(BWHKMKT-BWHKEY),BWHKEY                                     
*                                                                               
         BAS   RE,UPDTSAR          UPDATE TSAR RECORDS                          
*                                                                               
WRTHDRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE SPILL ELEMENTS FROM CAMPAIGN/MARKET HEADER RECORD            *         
***********************************************************************         
         SPACE 1                                                                
DELELS   NTR1  ,                                                                
         L     R2,AIOAREA3                                                      
         USING BWHRECD,R2                                                       
         LA    R8,BWHEL            FIND SPILL ELEMENTS                          
         USING SPLELD,R8                                                        
         SR    R0,R0                                                            
*                                                                               
DEL2     CLI   0(R8),0                                                          
         BE    DELX                                                             
         CLI   0(R8),SPLELCDQ                                                   
         BNE   DEL10                                                            
         OC    QSTA,QSTA           FOUND-TEST STATION FILTER                    
         BZ    *+18                                                             
         CLC   SPLSTA(4),QSTA      YES-MATCH THE STATION                        
         BNE   DEL10                                                            
         BE    DEL6                                                             
         L     R1,AIOAREA1         NO-MATCH STATION TO STATIONS IN              
         LA    R1,BWHFSTEL-BWHRECD(R1)  ORIGINATING MARKET'S HEADER             
         SR    RF,RF                                                            
*                                                                               
DEL4     CLI   0(R1),0                                                          
         BE    DEL10                                                            
         CLI   0(R1),BWHELCDQ                                                   
         BNE   *+14                                                             
         USING BWHEL,R1                                                         
         CLC   SPLSTA(4),BWHSTA                                                 
         BE    DEL6                MATCH FOUND                                  
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     DEL4                                                             
*                                                                               
DEL6     CLI   BDPT,0              TEST DAYPART FILTER                          
         BE    *+14                                                             
         CLC   SPLDPT,BDPT         YES-MATCH DAYPART                            
         BNE   DEL10                                                            
         CLI   BSLN,0              TEST SPOT LENGTH FILTER                      
         BE    DEL8                                                             
         CLC   SPLSLN,BSLN         YES-MATCH SPOT LENGTH                        
         BE    DEL8                                                             
         CLI   CMPSLN,0            TEST CAMPAIGN HAS LENGTH FILTER              
         BE    DEL10                                                            
         CLC   SPLSLN,CMPSLN       YES-DELETE THOSE ELEMENTS THAT DON'T         
         BE    DEL10                   MATCH CAMPAIGN SPOT LENGTH               
*                                                                               
DEL8     MVI   APELEM,X'FF'        DELETE SPILL ELEMENT                         
         MVI   0(R8),X'FF'                                                      
         GOTO1 ADELELS,BWHRECD                                                  
         B     DEL2                                                             
*                                                                               
DEL10    IC    R0,1(R8)            NEXT ELEMENT                                 
         AR    R8,R0                                                            
         B     DEL2                                                             
*                                                                               
DELX     B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE SPILL/NET TSAR RECORDS                                       *         
***********************************************************************         
         SPACE 1                                                                
UPDTSAR  NTR1  ,                                                                
         L     R2,AIOAREA3                                                      
         USING BWHRECD,R2                                                       
         TM    TWAINDS,TWAITSIN    TEST TSAR INITIALIZED                        
         BZ    UPDX                                                             
         LA    R4,LTSARREC         YES-BUILD TSAR RECORD KEY                    
         USING TRECD,R4                                                         
         LR    RE,R4                                                            
         LA    RF,TSPLRECL                                                      
         XCEFL                                                                  
*                                                                               
         MVI   TRECTYP,TRECSPL                                                  
         MVC   TAGYMD,BAGYMD                                                    
         MVC   TBYR,BBYR                                                        
         MVC   TCAM,BCAM                                                        
         MVC   TMKT,BWHKMKT        SPILL MARKET                                 
         MVC   LKEYSAVE,TKEY                                                    
         MVI   TSARACT,TSARDH                                                   
         GOTO1 ATSAR,TREC          READ A TOTAL RECORD                          
         BE    UPD0                                                             
         CLC   FVMSGNO,=AL2(FVTMR)                                              
         BE    UPDX                                                             
         CLC   FVMSGNO,=AL2(FVFERNF)                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TKEY(TTYPE-TKEY),LKEYSAVE                                        
         BNE   UPDX                                                             
*                                                                               
UPD0     MVC   TKEY,LKEYSAVE                                                    
         MVI   TTYPE,C'T'                                                       
         MVC   LKEYSAVE,TKEY                                                    
*                                                                               
UPD1     MVI   TSARACT,TSARDH      READ TV SPILL RECORDS                        
         GOTO1 ATSAR,TREC                                                       
         BE    UPD2                                                             
         CLC   FVMSGNO,=AL2(FVTMR) TEST EOF                                     
         BE    UPD4                                                             
         CLC   FVMSGNO,=AL2(FVFERNF)  TEST EXACT KEY NOT FOUND                  
         BE    UPD2                                                             
         DC    H'0'                                                             
*                                                                               
UPD2     CLC   TKEY(TDPT-TKEY),LKEYSAVE                                         
         BNE   UPD4                                                             
         MVI   TSARACT,TSADEL      DELETE                                       
         GOTO1 ATSAR,TREC                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TKEY,LKEYSAVE                                                    
         B     UPD1                GET NEXT TV SPILL RECORD                     
*                                                                               
UPD4     LA    R8,BWHFSTEL         GET ALL SPILL ELEMENTS                       
         SR    R0,R0                                                            
*                                                                               
UPD6     CLI   0(R8),0                                                          
         BE    UPDX                                                             
         CLI   0(R8),SPLELCDQ                                                   
         BNE   UPD8                                                             
         USING SPLELD,R8                                                        
         XC    LDEMS(4*53),LDEMS                                                
         ZIC   RE,SPLELLN                                                       
         SH    RE,=Y(SPLDEMS-SPLELD)                                            
         BCTR  RE,0                                                             
         EX    RE,*+4              EXTRACT DEMOS                                
         MVC   LDEMS(0),SPLDEMS                                                 
         LA    RE,1(RE)                                                         
         SRL   RE,2                                                             
         SR    R1,R1                                                            
         LA    RF,LDEMS                                                         
         A     R1,0(RF)            DEMO TOTAL                                   
         LA    RF,4(RF)                                                         
         BCT   RE,*-8                                                           
         ST    R1,LDEMTOT                                                       
         MVC   TKEY,LKEYSAVE                                                    
         MVC   TDPT,SPLDPT                                                      
         MVC   TSLN,SPLSLN                                                      
         BAS   RE,ADDREC           ADD TSAR RECORD FOR THIS DPT/LEN             
         MVI   TDPT,0                                                           
         MVI   TSLN,0                                                           
         BAS   RE,ADDREC           ADD TV SPILL TOTAL RECORD                    
*                                                                               
UPD8     IC    R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     UPD6                                                             
*                                                                               
UPDX     MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A RECORD TO TSAR                                     *         
***********************************************************************         
         SPACE 1                                                                
ADDREC   NTR1  ,                                                                
         LA    R4,LTSARREC                                                      
         USING TRECD,R4                                                         
         MVC   LKEYSAV2,TKEY                                                    
         MVI   TSARACT,TSARDH      SEE IF ALREADY EXISTS                        
         GOTO1 ATSAR,TREC                                                       
         BE    ADD2                YES                                          
         MVC   TKEY,LKEYSAV2       NO-MOVE BACK THE KEY                         
         XC    TCOST,TCOST         AND MOVE THE VALUES                          
         MVC   TDEMTOT,LDEMTOT                                                  
         XC    TDOLS(4*53),TDOLS                                                
         MVC   TDEMS(4*53),LDEMS                                                
         MVI   TSARACT,TSAADD      ADD TSAR RECORD                              
         B     ADD4                                                             
*                                                                               
ADD2     ICM   R1,15,TDEMTOT       ADD IN THIS RECORD'S VALUES                  
         A     R1,LDEMTOT                                                       
         STCM  R1,15,TDEMTOT                                                    
         LA    RE,LDEMS                                                         
         LA    RF,TDEMS                                                         
         ZIC   R0,CMPNWKS                                                       
*                                                                               
ADD3     L     R1,0(RF)                                                         
         A     R1,0(RE)                                                         
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,ADD3                                                          
         MVI   TSARACT,TSAWRT      WRITE TSAR RECORD                            
*                                                                               
ADD4     GOTO1 ATSAR,TREC          ADD/UPDATE TSAR RECORD                       
         BE    ADDX                                                             
         DC    H'0'                                                             
*                                                                               
ADDX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET THE SPILL MARKETS                                               *         
***********************************************************************         
         SPACE 1                                                                
GETMKTS  NTR1  ,                                                                
         LA    R1,LMKTS            CLEAR SPILL MARKETS TABLE                    
         LA    RF,MAXSTAS                                                       
         XC    0(L'LMKTS,R1),0(R1)                                              
         LA    R1,L'LMKTS(R1)                                                   
         BCT   RF,*-10                                                          
         XC    SPLMKTS,SPLMKTS                                                  
*                                                                               
         L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         LA    R3,BWHFSTEL         LOOP ROUND STATIONS IN HEADER                
         SR    R4,R4                                                            
         LA    R8,LMKTS                                                         
*                                                                               
GETM2    CLI   0(R3),0                                                          
         BE    GETM15                                                           
         CLI   0(R3),BWHELCDQ                                                   
         BNE   GETM14                                                           
         USING BWHEL,R3                                                         
         CLC   BWHSTA,SPACES                                                    
         BNH   GETM14                                                           
         MVC   0(4,R8),BWHSTA      SAVE STATION IN TABLE                        
         LA    R9,4(R8)                                                         
         LA    R0,MAXMKTS+1                                                     
         LA    R6,IOKEY            BUILD SPILL RECORD KEY                       
         USING SDEFRECD,R6                                                      
         XC    SDEFKEY,SDEFKEY                                                  
         MVC   SDEFKTYP,=X'0D13'                                                
         MVC   SDEFKAGY,CUAALF                                                  
         MVI   SDEFKRSV,C'0'                                                    
         TM    APROFBTS,A00CANAD   CANADIAN?                                    
         BO    GETM3C               - YUP YUP                                   
         CLI   CLTSRC,C'A'         0=NSI,1=ARB                                  
         BNE   *+8                                                              
         MVI   SDEFKRSV,C'1'                                                    
         B     GETM3G                                                           
*                                                                               
GETM3C   CLI   CLTSRCDF,C'A'       0=NSI,1=ARB                                  
         BNE   *+8                                                              
         MVI   SDEFKRSV,C'1'                                                    
*                                                                               
GETM3G   MVC   SDEFKSTA(4),BWHSTA                                               
         MVC   SDEFKCLT,BCLT       TRY CLIENT SPECIFIC FIRST                    
*                                                                               
GETM4    GOTO1 AIO,DIRHI+IO2       READ SPILL RECORD POINTER                    
         BNE   *+14                                                             
         CLC   SDEFKEY(13),IOKEYSAV                                             
         BE    GETM6                                                            
         MVC   IOKEY,IOKEYSAV                                                   
         OC    SDEFKCLT,SDEFKCLT                                                
         BZ    GETM12                                                           
         XC    SDEFKCLT,SDEFKCLT   TRY NOT CLIENT SPECIFIC                      
         B     GETM4                                                            
*                                                                               
GETM6    GOTO1 AIO,FILGET2         GET SPILL RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIOAREA2                                                      
         CLC   SDEFLEN,=H'256'     RECORD LENGTH MUST BE LE 256                 
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    R1,SDEFEL                                                        
         USING SDEFEL05,R1                                                      
         SR    RF,RF               LOOK FOR SPILL MARKET ELEMENTS               
*                                                                               
GETM8    CLI   0(R1),0             TEST END OF SPILL RECORD                     
         BE    GETM12                                                           
         CLI   0(R1),5             TEST SPILL MARKET ELEMENT                    
         BNE   GETM11                                                           
         CLC   SDEFAMKT,BMKT       TEST SPILL MKT EQUAL ACTUAL MKT              
         BE    GETM11              YES - SKIP                                   
         TM    SDEFCEX,X'80'       TEST '*' FEATURE                             
         BO    GETM11              YES - SKIP                                   
         MVC   0(2,R9),SDEFAMKT    SAVE AGENCY MARKET                           
         LA    RE,SPLMKTS                                                       
         LA    R6,L'SPLMKTS/2      SAVE IN TABLE OF SPILL MARKETS               
*                                                                               
GETM9    OC    0(2,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   0(2,RE),SDEFAMKT                                                 
         B     GETM10                                                           
         CLC   SDEFAMKT,0(RE)                                                   
         BE    GETM10                                                           
         LA    RE,2(RE)                                                         
         BCT   R6,GETM9                                                         
         DC    H'0'                                                             
*                                                                               
GETM10   MVC   2(2,R9),SDEFRMKT    SAVE RATING SVC MARKET                       
         MVC   4(1,R9),SDEFBKTY    SAVE SPECIAL BOOK TYPE                       
         LA    R9,5(R9)                                                         
         BCT   R0,GETM11                                                        
         DC    H'0'                EXPAND MAXMKTS                               
*                                                                               
GETM11   IC    RF,1(R1)            NEXT ELEMENT                                 
         AR    R1,RF                                                            
         B     GETM8                                                            
*                                                                               
GETM12   LA    R8,L'LMKTS(R8)                                                   
*                                                                               
GETM14   IC    R4,1(R3)            NEXT STATION                                 
         AR    R3,R4                                                            
         B     GETM2                                                            
*                                                                               
GETM15   MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R8,LMKTS            CHECK THERE ARE ANY SPILL MARKETS            
         LA    R4,MAXSTAS                                                       
*                                                                               
GETM16   OC    0(4,R8),0(R8)                                                    
         BZ    GETM99                                                           
         OC    4(4,R8),4(R8)                                                    
         BNZ   GETMX                                                            
         LA    R8,L'LMKTS(R8)                                                   
         BCT   R4,GETM16                                                        
*                                                                               
GETM99   MVC   FVMSGNO,=AL2(FVNOSPL)                                            
*                                                                               
GETMX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
EFFS     DC    XL2'FFFF'                                                        
SPACES   DC    CL80' '                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* SPNWSWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
LOCALD   DSECT                                                                  
*                                                                               
COMWRK   DS    0C                  COMMON BETWEEN BWS05 AND BWS09               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPNWS05WRK                                                     
         PRINT ON                                                               
         SPACE 2                                                                
LOCALD   DSECT                                                                  
         ORG   LOCALD+2048         BWS09 WORK AREA                              
*                                                                               
         DS    0D                                                               
LASPWEL  DS    A                                                                
LASPIEL  DS    A                                                                
LADEMTAB DS    A                                                                
LNENTS   DS    F                                                                
LDEM     DS    XL4                                                              
LDEMVAL  DS    XL4                                                              
LDEMTOT  DS    XL4                                                              
LDEMS    DS    53XL4                                                            
LNEWHDR  DS    CL1                                                              
LUPPUT   DS    CL1                                                              
LUPSHR   DS    CL1                                                              
LPSVKEY  DS    XL13                                                             
LTSARREC DS    XL(TSPLRECL)                                                     
LKEYSAVE DS    XL(L'TKEY)                                                       
LKEYSAV2 DS    XL(L'TKEY)                                                       
LDMUPBLK DS    (SPDEMUP2)X                                                      
         SPACE 2                                                                
* TABLE OF SPILL MARKETS                                                        
* ONE ENTRY PER STATION                                                         
* ENTRY=STATION(4)/SPILL MARKETS - 5 BYTES PER MARKET                           
*                                  +0(2)=AGENCY MARKET                          
*                                  +2(2)=RATING SVC MARKET                      
*                                  +4(1)=SPECIAL BOOK TYPE                      
*                                                                               
LMKTS    DS    (MAXSTAS)XL(4+5*MAXMKTS)                                         
MAXMKTS  EQU   12                  MAXIMUM SPILL MARKETS PER STATION            
MAXSTAS  EQU   12                  MAXIMUM SPILL STATIONS                       
         SPACE 2                                                                
SPLMKTS  DS    XL32                                                             
         SPACE 2                                                                
* DSECT FOR DEMO TABLE ENTRY                                                    
*                                                                               
DRECD    DSECT                                                                  
DKEY     DS    0CL8                                                             
DMKT     DS    XL2                                                              
DDPT     DS    CL1                                                              
DSLN     DS    XL1                                                              
DSTA     DS    XL4                                                              
DDEMS    DS    53XL4                                                            
DRECL    EQU   *-DRECD         ENTRY USED TO BE 64 BYTES NOW 220 BYTES          
*                                                                               
***MAXENTS  EQU   100                 MAXIMUM N'ENTRIES  (64*100)               
MAXENTS  EQU   29                   (64*100/220)                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
* SPNWSFBD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSFBD                                                       
         PRINT ON                                                               
         EJECT                                                                  
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENSDEF                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENSDEF                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPNWS09   02/26/07'                                      
         END                                                                    
