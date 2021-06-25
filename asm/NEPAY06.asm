*          DATA SET NEPAY06    AT LEVEL 021 AS OF 11/20/17                      
*PHASE T31306A                                                                  
         TITLE 'NETPAK PAY PROGRAM - AUDIT TRAIL OVERLAY - T31306'              
T31306   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AUDT**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING PAYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         L     R6,ASPOOLA                                                       
         USING SPOOLD,R6                                                        
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         SPACE 2                                                                
* EDIT PERIOD FIELD                                                             
*                                                                               
AU       LA    R2,PAYPERH                                                       
         ST    R2,FADDR                                                         
         TM    MODE,FIRST                                                       
         BZ    *+8                                                              
         OI    MODE,DISPLAY        FIRST TIME MEANS START DISPLAY               
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,STAR                                                       
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BNE   AU4                                                              
*                                                                               
AU3      MVI   FERN,MISERR                                                      
         MVC   XTRA(9),=C'UNIT DATE'                                            
         B     ERROR                                                            
         SPACE                                                                  
AU4      MVI   FERN,DATERR                                                      
         GOTO1 VDATVAL,DMCB,(0,FLD),DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         CLC   FLDH+5(1),3(R1)     TEST IF DATE MAKES UP WHOLE FIELD            
         BNE   ERROR               NO                                           
         SPACE                                                                  
         MVC   START,DUB                                                        
         MVC   END,DUB                                                          
         CLI   FSTOP,STAR          TEST IF STAR FOUND                           
         BNE   AU10                NO                                           
*                                                                               
         XC    FTERM,FTERM                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    AU7                                                              
         TM    FLDH+4,X'08'                                                     
         BZ    AU7                                                              
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    AU7                                                              
         CH    R0,=H'255'                                                       
         BH    AU7                                                              
         STC   R0,SUB                                                           
         B     AU10                                                             
         SPACE                                                                  
AU7      MVI   FERN,INVERR                                                      
         MVC   XTRA(8),=C'SUB-LINE'                                             
         B     ERROR                                                            
         SPACE 2                                                                
* TEST FOR CHANGE IN CONTROL VALUES - INITIALIZE                                
*                                                                               
AU10     CLC   THISVALS,LASTVALS                                                
         BE    *+8                                                              
         OI    MODE,DISPLAY                                                     
         MVC   LASTVALS(LASTLEN),THISVALS                                       
         GOTO1 VCLEARF,DMCB,(1,AUDLIN1H),AUDLAST                                
         TM    MODE,DISPLAY                                                     
         BO    AU12                                                             
         BAS   RE,GETSAVE                                                       
         B     AU20                                                             
         SPACE                                                                  
AU12     BAS   RE,GETUNIT          GET THE UNIT                                 
         MVC   DMCB+4(4),=X'D9000A12'  GET A(XSORT)                             
         GOTO1 VCALOV,DMCB,0                                                    
         MVC   VXSORT,DMCB                                                      
         MVI   SVTABNTR,0          ZERO TABLE ENTRY COUNT                       
         MVI   SVLNTRY,0           AND POINTER TO LAST ENTRY DISPLAYED          
         MVC   FLD,SPACES                                                       
         TM    NBUNITST,X'40'      TEST FOR PRE-EMPTED UNIT                     
         BZ    AU13                                                             
         MVC   FLD+14(18),=C'UNIT IS PRE-EMPTED'                                
         BAS   RE,ADDLIN                                                        
         B     AU14                                                             
         SPACE                                                                  
AU13     TM    NBUNITST,X'02'      TEST FOR MISSED UNIT                         
         BZ    AU15                NOT MISSED OR PRE-EMPTED                     
         MVC   FLD+14(14),=C'UNIT IS MISSED'                                    
         MVC   FLD+29(14),=C'- MADE-GOOD BY'                                    
         MVC   FLD+44(6),NBMGBPCD                                               
         GOTO1 VDATCON,DMCB,(2,NBMGBDAT),(4,FLD+51)                             
         CLI   NBMGBSUB,0                                                       
         BE    AU13A                                                            
         CLI   NBMGBSUB,1                                                       
         BE    AU13A                                                            
         ZIC   R0,NBMGBSUB                                                      
         MVI   FLD+56,DASH                                                      
         EDIT  (R0),(3,FLD+57),ALIGN=LEFT                                       
*                                                                               
AU13A    BAS   RE,ADDLIN                                                        
         SPACE                                                                  
AU14     BAS   RE,ADDLIN           SKIP A LINE AFTER MISSED/PREEMPT             
*                                                                               
AU15     BAS   RE,FBILL            FORMAT BILLING DETAILS                       
         BAS   RE,ADDLIN           SKIP A LINE                                  
         BAS   RE,FPAY             FORMAT PAYING DETAILS                        
         BAS   RE,PUTSAVE          SAVE SCREEN OUTPUT                           
         SPACE                                                                  
* OUTPUT THE NEXT SCREEN                                                        
*                                                                               
AU20     LA    R2,AUDLIN1          R2 POINTS TO DATA LINE                       
         L     R4,AIOAREA2         POINT AT OUTPUT TABLE                        
         ZIC   RE,SVLNTRY          GET INDEX TO LAST DISPLAYED ENTRY            
         LR    R1,RE                                                            
         MH    RE,=Y(L'AUDLIN1)    DISPLACEMENT TO NEXT ENTRY                   
         LA    R4,0(R4,RE)         POINT R4 AT IT                               
         ZIC   R3,SVTABNTR         COMPUTE NUMBER OF LINES ON                   
         SR    R3,R1               THIS SCREEN                                  
         LA    R0,LINES                                                         
         CR    R3,R0                                                            
         BL    *+6                                                              
         LR    R3,R0                                                            
         SPACE                                                                  
AU22     MVC   0(L'AUDLIN1,R2),0(R4)                                            
         LA    R2,LINELEN(R2)                                                   
         LA    R4,L'AUDLIN1(R4)                                                 
         ZIC   R1,SVLNTRY                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SVLNTRY                                                       
         BCT   R3,AU22                                                          
         SPACE 2                                                                
* END OF SCREEN PROCESSING                                                      
*                                                                               
AU30     MVI   MORESW,YES                                                       
         CLC   SVLNTRY,SVTABNTR                                                 
         BL    *+8                                                              
         MVI   MORESW,NO                                                        
*                                                                               
         MVC   PAYMSG(27),=C'AUDIT TRAIL DATA DISPLAYED.'                       
         MVC   PAYMSG+29(20),=C'PRESS ENTER FOR MORE'                           
         CLI   MORESW,YES                                                       
         BE    AU35                                                             
         MVC   PAYMSG+29(20),=CL20'ENTER NEXT REQUEST'                          
         XC    LASTVALS(LASTLEN),LASTVALS                                       
         B     AU35                                                             
         SPACE                                                                  
AU35     LA    R2,PAYACTH                                                       
         OI    6(R2),X'01'                                                      
         ST    R2,FADDR                                                         
         NI    MODE,X'FF'-FIRST-DISPLAY                                         
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO GET UNIT RECORD                                                
*                                                                               
GETUNIT  ST    RE,SAVEREG                                                       
         MVC   NBSELSTR,START                                                   
         MVC   NBSELEND,NBSELSTR                                                
         MVC   NBSELSUB,SUB                                                     
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBDIRECT,YES                                                     
         MVI   NBSELPST,C'B'                                                    
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBUSER+13,NO        **FUDGE PROFILE TO RETURN PRE-EMPTS          
         MVC   NBAIO,AIOAREA1                                                   
         MVI   FERN,NOTFOUND                                                    
*READ PACKAGE RECORD TO GET DAYPART                                             
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING NPRECD,RE                                                        
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM(3),NBACTAM                                                 
         MVC   NPKNET,NBSELNET                                                  
         MVC   NPKEST,NBSELEST                                                  
         MVC   NPKPACK,NBSELPAK                                                 
*                                                                               
         GOTO1 AIO,DMCB,UNT+HIGH+DIR                                            
         CLC   KEY(20),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         GOTO1 AIO,DMCB,UNT+GET+FILE,NBAIO                                      
         L     RE,NBAIO                                                         
         MVC   NBSELDP,NPAKDP                                                   
         DROP  RE                                                               
         SPACE                                                                  
GETUNIT2 GOTO1 VNETIO,DMCB,NETBLOCK                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RETURNED                       
         BE    GETUNITX            YES                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    ERROR                                                            
         B     GETUNIT2                                                         
         SPACE                                                                  
GETUNITX L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*  READ BILL ELEMENTS FROM NEW BILL RECORDS                                     
*  DISPLAY THE BILL INFORMATION                                                 
*                                                                               
FBILL    NTR1                                                                   
*  INITIALIZE THE BILL TABLE                                                    
*                                                                               
         XC    BILENT,BILENT       ZERO COUNT OF BILLING ENTRIES                
         L     R1,NBAIO                                                         
         LA    R1,NUMAINEL-NUKEY(R1)                                            
         SR    R0,R0                                                            
         LA    R4,TABLE            R4 POINTS TO TABLE OF BILLING DATA           
         USING BILTABD,R4                                                       
*  SET UP BLOCK TO READ BILL ELEMENTS                                           
*                                                                               
         XC    BUFFER,BUFFER                                                    
         LA    RE,BUFFER                                                        
         STCM  RE,15,NBABILRD       ADDRESS OF BILL DSECT IN NETBLOCK           
         USING NBLBILLD,RE                                                      
         MVC   NBLUNAIO,NBAIO                                                   
         OI    NBLFUNC,NBLHOOK2                                                 
         LA    R1,BILLHOOK                                                      
         ST    R1,NBLAHOOK                                                      
         LA    R1,BUFFER+100                                                    
         ST    R1,NBL10AIO                                                      
         GOTO1 VBILRDR,DMCB,NETBLOCK                                            
*                                                                               
*  DISPALY BILLING INFORMATION                                                  
*                                                                               
         MVC   FLD+24(21),=C'** BILLING DETAILS **'                             
*                                                                               
         BAS   RE,ADDLIN                                                        
         BAS   RE,ADDLIN                                                        
         MVC   FLD+14(13),=C'ASSIGNED COST'                                     
         L     R0,NBASSIGN                                                      
         TM    NBUNITST,X'80'      TEST FOR MINUS UNIT                          
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         BAS   RE,EDCOS1                                                        
         MVI   FLD+30,EQUALS                                                    
         BAS   RE,ADDLIN                                                        
*                                                                               
         BAS   RE,ACTCOST          DISPLAY THE ACTUAL COST                      
*                                                                               
         MVC   FLD+14(11),=C'INTEGRATION'                                       
         MVI   FLD+30,EQUALS                                                    
         L     R0,NBINTEG                                                       
         TM    NBUNITST,X'80'                                                   
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         BAS   RE,EDCOS1                                                        
         BAS   RE,ADDLIN                                                        
*                                                                               
         BAS   RE,SPECCOST                                                      
*                                                                               
**       CLI   NBPRD,0                                                          
**       BE    FBILL2                                                           
         OC    NBPR1CL3,NBPR1CL3                                                
         BZ    FBILL2                                                           
         MVC   FLD+14(10),=C'ALLOCATION'                                        
**       MVC   BYTE,NBPRD                                                       
**       BAS   RE,GETPRD                                                        
         MVI   FLD+30,EQUALS                                                    
**       MVC   FLD+32(3),THREE                                                  
         MVC   FLD+32(3),NBPR1CL3                                               
**       CLI   NBPRD2,0            TEST FOR SECOND PRODUCT                      
**       BNE   FBILL1              YES                                          
         OC    NBPR2CL3,NBPR2CL3                                                
         BNZ   FBILL1                                                           
         BAS   RE,ADDLIN           NO-WRAP UP FOR ONE BRAND ONLY                
         B     FBILL2                                                           
         SPACE 1                                                                
FBILL1   DS    0H                                                               
**       MVC   BYTE,NBPRD2                                                      
**       BAS   RE,GETPRD                                                        
         LA    R1,FLD+34                                                        
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),COMMA                                                      
         LA    R1,1(R1)                                                         
**       MVC   0(3,R1),THREE                                                    
         MVC   0(3,R1),NBPR2CL3                                                 
         BAS   RE,ADDLIN                                                        
*                                                                               
         MVC   FLD+14(12),=C'FIRST PRD. %'                                      
         MVI   FLD+30,EQUALS                                                    
         SR    R0,R0                                                            
         ICM   R0,3,NBP1SHR                                                     
         EDIT  (R0),(6,FLD+32),2,ALIGN=LEFT                                     
         BAS   RE,ADDLIN                                                        
         SPACE                                                                  
FBILL2   MVC   FLD+14(11),=C'BILLED TIME'                                       
         L     R0,BILLTIME                                                      
         BAS   RE,EDCOS1                                                        
         MVI   FLD+30,EQUALS                                                    
         BAS   RE,ADDLIN                                                        
*                                                                               
         MVC   FLD+14(11),=C'BILLED INT.'                                       
         L     R0,BILLINTG                                                      
         BAS   RE,EDCOS1                                                        
         MVI   FLD+30,EQUALS                                                    
         BAS   RE,ADDLIN                                                        
*                                                                               
         MVC   FLD+14(12),=C'BILLED SPEC.'                                      
         L     R0,BILLSPEC                                                      
         BAS   RE,EDCOS1                                                        
         MVI   FLD+30,EQUALS                                                    
         BAS   RE,ADDLIN                                                        
*                                                                               
*  SORT THE RECORDS                                                             
*                                                                               
         ICM   R2,15,BILENT        SORT THE TABLE                               
         BZ    FBILLX              NO BILLING ELEMENTS                          
         GOTO1 VXSORT,DMCB,TABLE,(R2),BILTABL,BILKEYL,BILKEY-BILTABD            
         LA    R4,TABLE            POINT TO FIRST ENTRY FOR DISPLAY             
         BAS   RE,ADDLIN                                                        
         MVC   FLD(L'BILHED),BILHED SET BILLING HEADLINE                        
         MVC   FLD+L'BILHED(L'BILHED2),BILHED2                                  
         BAS   RE,ADDLIN                                                        
         SPACE                                                                  
FBILL10  GOTO1 VDATCON,DMCB,(2,BILDAT),(4,FLD+1)                                
         LA    RE,CONVTAB          CONVERT BILL TYPE                            
FBILL10A CLI   0(RE),X'FF'         INVALID BILL TYPE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BILTYP(1),0(RE)                                                  
         BE    FBILL10B                                                         
         LA    RE,5(RE)                                                         
         B     FBILL10A                                                         
FBILL10B MVC   FLD+8(4),1(RE)                                                   
*                                                                               
         MVC   BYTE,BILPRD                                                      
         BAS   RE,GETPRD                                                        
         MVC   FLD+14(3),THREE                                                  
         OC    BILNUM,BILNUM       TEST FOR A BILL NUMBER                       
         BZ    FBILL11             NO-MUST BE CONVERTED UNIT                    
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BILDAT),DUB                                      
         MVC   FLD+23(2),DUB+2                                                  
         MVI   FLD+25,DASH                                                      
         MVC   FLD+26(4),BILNUM                                                 
*                                                                               
FBILL11  L     R0,BILGROSS                                                      
         BAS   RE,EDCOS2                                                        
         L     R0,BILNET                                                        
         BAS   RE,EDCOS3                                                        
         MVC   FLD+56(L'BILFORM),BILFORM                                        
         TM    BILST,REVERSED      NOTE REVERSED AND REVERSAL BILLING           
         BZ    *+14                                                             
         MVC   FLD+63(8),=C'REVERSED'                                           
         B     FBILL12                                                          
         TM    BILST,REVERSAL                                                   
         BZ    *+10                                                             
         MVC   FLD+63(8),=C'REVERSAL'                                           
         SPACE                                                                  
FBILL12  BAS   RE,ADDLIN                                                        
         LA    R4,BILTABL(R4)      NEXT BILLING ENTRY                           
         BCT   R2,FBILL10                                                       
         SPACE                                                                  
FBILLX   B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO FORMAT BILLING DETAILS                                         
*                                                                               
BILLHOOK NTR1                                                                   
         LA    R4,TABLE                                                         
         SR    RE,RE                                                            
         LA    RE,BILTABL                                                       
         MH    RE,BILENT+2                                                      
         AR    R4,RE                POSITION THE POINTER                        
*                                                                               
         BAS   RE,ADDBILL          ACCUMULATE GROSS BILLING                     
*                                                                               
         SPACE                                                                  
         LA    R1,BUFFER+100        R1 POINTS TO BILL ELEMENT                   
         USING NUBILD,R1                                                        
         TM    NUBILST,UNBILL      TEST FOR UNBILLED ELEMENT                    
         BO    BILLHEX             YES-DO NOT DISPLAY IT                        
         XC    BILTABD(BILTABL),BILTABD CLEAR NEXT ENTRY                        
         MVC   BILDAT,NUBILDAT                                                  
         LA    RE,SORTTAB          CONVERT BILL TYPE                            
BILLH5A  CLI   0(RE),X'FF'         INVALID BILL TYPE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NUBILTYP(1),0(RE)                                                
         BE    BILLH5B                                                          
         LA    RE,2(RE)                                                         
         B     BILLH5A                                                          
BILLH5B  MVC   BILTYP(1),1(RE)                                                  
*                                                                               
         MVC   BILPRD,NUBILPRD                                                  
         MVC   BILNUM,NUBILNUM                                                  
         MVC   BILGROSS,NUBILGRS                                                
         MVC   BILNET,NUBILNET                                                  
         MVC   BILST,NUBILST                                                    
         MVC   BILFORM,NUBILBTY                                                 
         L     RE,BILENT                                                        
         LA    RE,1(RE)            INCREMENT BILLING ENTRY COUNT                
         ST    RE,BILENT                                                        
         LA    R4,BILTABL(R4)      NEXT ENTRY                                   
BILLHEX  B     EXXMOD                                                           
         DROP  R1,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO FORMAT PAYING DETAILS                                          
*                                                                               
FPAY     NTR1                                                                   
         MVC   FLD+24(20),=C'** PAYING DETAILS **'                              
         BAS   RE,ADDLIN                                                        
         BAS   RE,ADDLIN                                                        
*                                                                               
         BAS   RE,ACTCOST                                                       
*                                                                               
FPAY1    MVC   FLD+14(11),=C'INTEGRATION'                                       
         MVI   FLD+30,EQUALS                                                    
         L     R0,NBINTEG                                                       
         TM    NBUNITST,X'80'                                                   
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         BAS   RE,EDCOS1                                                        
         BAS   RE,ADDLIN                                                        
*                                                                               
         BAS   RE,SPECCOST                                                      
*                                                                               
         MVC   FLD+14(12),=C'CLEARED TIME'                                      
         MVI   FLD+30,EQUALS                                                    
         L     R0,NBPAYTGR                                                      
         BAS   RE,EDCOS1                                                        
         BAS   RE,ADDLIN                                                        
*                                                                               
         MVC   FLD+14(11),=C'CLEARED INT'                                       
         MVI   FLD+30,EQUALS                                                    
         L     R0,NBPAYIGR                                                      
         BAS   RE,EDCOS1                                                        
         BAS   RE,ADDLIN                                                        
*                                                                               
         BAS   RE,PAYSPEC          CLEARED SPECIALS                             
*                                                                               
FPAY2    XC    PAYENT,PAYENT                                                    
         L     R1,NBAIO                                                         
         LA    R1,NUMAINEL-NUKEY(R1)                                            
         SR    R0,R0                                                            
         LA    R4,TABLE                                                         
         USING PAYTABD,R4                                                       
         SPACE                                                                  
FPAY4    CLI   0(R1),0                                                          
         BE    FPAY8                                                            
         CLI   0(R1),X'12'                                                      
         BNE   FPAY6                                                            
         USING NUPAYD,R1                                                        
         XC    PAYTABD(PAYTABL),PAYTABD CLEAR NEXT ENTRY                        
         MVC   PAYDAT,NUPAYDAT                                                  
         MVC   PAYSEQ,NUPAYSEQ                                                  
         LA    RE,SORTTAB          CONVERT PAY TYPE                             
FPAY4A   CLI   0(RE),X'FF'         INVALID PAY TYPE                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   NUPAYTYP(1),0(RE)                                                
         BE    FPAY4B                                                           
         LA    RE,2(RE)                                                         
         B     FPAY4A                                                           
FPAY4B   MVC   PAYTYP(1),1(RE)                                                  
*                                                                               
         OC    NUPAYREP,NUPAYREP   TEST FOR ANY REP                             
         BZ    FPAY5               NO                                           
         CLC   =C'000',NUPAYREP    TEST FOR STATION AS PAYEE                    
         BE    FPAY5               YES                                          
         LA    RE,PAYREP           POINT TO START OF FIELD                      
         CLI   NUPAYRTY,0          TEST FOR REP TYPE                            
         BE    *+14                NO                                           
         MVC   PAYREP(1),NUPAYRTY  REP TYPE COMES FIRST                         
         LA    RE,PAYREP+1         POINT PAST REP TYPE                          
         MVC   0(3,RE),NUPAYREP    THEN REP CODE                                
*                                                                               
FPAY5    MVC   PAYG(8),NUPAYGRS                                                 
         L     RE,PAYENT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,PAYENT                                                        
         LA    R4,PAYTABL(R4)                                                   
         SPACE                                                                  
FPAY6    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FPAY4                                                            
         SPACE                                                                  
FPAY8    ICM   R2,15,PAYENT                                                     
         BZ    FPAYX                                                            
*                                                                               
         GOTO1 VXSORT,DMCB,TABLE,(R2),PAYTABL,PAYKEYL,PAYKEY-PAYTABD            
         LA    R4,TABLE                                                         
         BAS   RE,ADDLIN                                                        
         MVC   FLD(L'PAYHED),PAYHED                                             
         MVC   FLD+L'PAYHED(L'PAYHED2),PAYHED2                                  
         BAS   RE,ADDLIN                                                        
         SPACE                                                                  
FPAY10   GOTO1 VDATCON,DMCB,(2,PAYDAT),(4,FLD+1)                                
         LA    RE,CONVTAB          CONVERT PAY TYPE                             
FPAY10A  CLI   0(RE),X'FF'         INVALID PAY TYPE                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PAYTYP(1),0(RE)                                                  
         BE    FPAY10B                                                          
         LA    RE,5(RE)                                                         
         B     FPAY10A                                                          
FPAY10B  MVC   FLD+8(4),1(RE)                                                   
*                                                                               
         MVC   FLD+16(4),PAYREP                                                 
         ICM   R0,15,PAYG                                                       
         BAS   RE,EDCOS2                                                        
         ICM   R0,15,PAYN                                                       
         BAS   RE,EDCOS3                                                        
         BAS   RE,GETCHKS                                                       
         SPACE                                                                  
FPAY12   BAS   RE,ADDLIN                                                        
FPAY13   LA    R4,PAYTABL(R4)                                                   
         BCT   R2,FPAY10                                                        
         SPACE                                                                  
FPAYX    B     EXXMOD                                                           
         DROP  R1,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO ADD LINES TO SAVE AREA                                         
*                                                                               
ADDLIN   ST    RE,SAVEREG          SAVE RETURN POINT                            
         L     RE,AIOAREA2         POINT TO START OF AREA                       
         ZIC   R1,SVTABNTR         GET COUNT OF ENTRIES                         
         CH    R1,=Y(MAXSAVE)      COMPARE AGAINST LIMIT                        
         BE    ADDLINX             NO MORE ENTRIES                              
         LR    R0,R1               SAVE ENTRY COUNT                             
         MH    R1,=Y(L'AUDLIN1)                                                 
         LA    RE,0(R1,RE)                                                      
         MVC   0(L'AUDLIN1,RE),FLD SLOT LINE OF OUTPUT IN                       
         LR    R1,R0               RESTORE ENTRY COUNT                          
         LA    R1,1(R1)            AND INCREMENT IT                             
         STC   R1,SVTABNTR                                                      
         SPACE                                                                  
ADDLINX  MVC   FLD,SPACES          CLEAR OUTPUT AREA                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY THE ACTUAL COST                                        
*                                                                               
ACTCOST  NTR1                                                                   
         MVC   FLD+14(11),=C'ACTUAL COST'                                       
         MVI   FLD+30,EQUALS                                                    
         TM    NBUNITST,X'20'      TEST FOR COST OVERRIDE                       
         BO    *+14                YES                                          
         MVC   FLD+32(8),=C'**NONE**'                                           
         B     ACTCOST2                                                         
*                                                                               
         L     R0,NBACTUAL                                                      
         TM    NBUNITST,X'80'                                                   
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         BAS   RE,EDCOS1                                                        
*                                                                               
ACTCOST2 BAS   RE,ADDLIN                                                        
         B     EXXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO GET CHECK NUMBER AND CHECK DATE                                
* FROM CLEARENCE STATUS RECORD.                                                 
*                                                                               
GETCHKS  NTR1                                                                   
         USING PAYTABD,R4                                                       
         LA    R5,KEY                                                           
         USING CLRSTATD,R5                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   CLSKTYPE,=X'0D76'                                                
         MVC   CLSKAGMD,AGYMED     A-M                                          
         MVC   CLSKCLT,CLIPK       CLT                                          
         MVC   DUB(4),NETWORK                                                   
         MVI   DUB+4,C'N'                                                       
         GOTO1 VMSPACK,DMCB,MARKET,DUB,CLSKMKT                                  
*                                                                               
         GOTO1 AIO,DMCB,SPT+HIGH+DIR                                            
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GETCHEX                                                          
         MVC   SVKEY,KEY                                                        
*                                                                               
GETCH050 GOTO1 AIO,DMCB,SPT+SEQ+DIR                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GETCH080                                                         
         CLC   CLSKDATE,PAYDAT     CHECK CLEARENCE DATE                         
         BH    GETCH080                                                         
         BNE   *+14                IF DATES EQUAL CHECK SEQUENCE NUMBRS         
         CLC   CLSKSEQ,PAYSEQ      CHECK CLEARENCE SEQUENCE                     
         BH    GETCH080                                                         
         MVC   SVKEY,KEY                                                        
         B     GETCH050                                                         
*                                                                               
GETCH080 L     R5,AIOAREA4                                                      
         MVC   KEY(13),SVKEY                                                    
         GOTO1 AIO,DMCB,SPT+READ+DIR                                            
         GOTO1 AIO,DMCB,SPT+GET+FILE,AIOAREA4                                   
*                                                                               
         LA    R6,CLSTEL01                                                      
         USING CLSTEL01,R6                                                      
         B     GETCH110                                                         
*                                                                               
GETCH100 ZIC   RE,CLSTEL01+1                                                    
         AR    R6,RE                                                            
GETCH110 CLI   CLSTEL01,3                                                       
         BE    GETCH100                                                         
         CLI   CLSTEL01,5                                                       
         BE    GETCH100                                                         
         CLI   CLSTEL01,X'F1'                                                   
         BE    GETCH100                                                         
         CLI   CLSTEL01,1                                                       
         BNE   GETCHEX                                                          
*                                                                               
         CLC   CLSTCLRD,PAYDAT                                                  
         BNE   GETCH100                                                         
         CLC   CLSTCLSQ,PAYSEQ                                                  
         BNE   GETCH100                                                         
*                                                                               
         MVC   FLD+57(6),CLSTCHK                                                
         TM    CLSTSTAT,X'80'      RECONCILED?                                  
         BNO   *+8                                                              
         MVI   FLD+63,C'*'                                                      
         GOTO1 VDATCON,DMCB,(2,CLSTCHDT),(5,FLD+66)                             
GETCHEX  B     EXXMOD                                                           
         DROP  R4,R5,R6                                                         
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY THE SPECIAL COST                                       
*                                                                               
SPECCOST NTR1                                                                   
         MVC   FLD+14(8),=C'SPECIALS'                                           
         MVI   FLD+30,EQUALS                                                    
*                                                                               
         SR    R0,R0                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   SPECCT80            YES                                          
         L     R4,12(R1)                                                        
         B     SPECCT20                                                         
         USING NUSPRD,R4                                                        
SPECCT10 ZIC   RE,NUSPRLEN                                                      
         AR    R4,RE                                                            
         CLI   NUSPREL,X'03'                                                    
         BNE   SPECCT80                                                         
*                                                                               
SPECCT20 ICM   RF,15,NUSPRAMT                                                   
         AR    R0,RF               ADD TO ACCUMULATOR                           
         B     SPECCT10                                                         
*                                                                               
SPECCT80 BAS   RE,EDCOS1                                                        
         BAS   RE,ADDLIN                                                        
         B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO ACCUMULATE BILLED DOLLARS                                      
*                                                                               
ADDBILL  NTR1                                                                   
         LA    R4,BUFFER+100        BILLING ELEMENT                             
         USING NUBILD,R4                                                        
         CLI   NUBILEL,X'10'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    NUBILST,UNBILL      TEST FOR UNBILLED ELEMENT                    
         BO    ADBLLEX             YES-EXIT                                     
         CLI   NUBILTYP,C'T'       TEST FOR TIME                                
         BNE   ADBLL020                                                         
         L     RE,BILLTIME                                                      
         A     RE,NUBILGRS                                                      
         ST    RE,BILLTIME                                                      
         B     ADBLLEX             EXIT                                         
ADBLL020 CLI   NUBILTYP,C'I'       TEST FOR INTEGRATION                         
         BNE   ADBLL040                                                         
         L     RE,BILLINTG                                                      
         A     RE,NUBILGRS                                                      
         ST    RE,BILLINTG                                                      
         B     ADBLLEX             EXIT                                         
*                                                                               
ADBLL040 L     RE,BILLSPEC          ADD TO SPECIAL                              
         A     RE,NUBILGRS                                                      
         ST    RE,BILLSPEC                                                      
*                                                                               
ADBLLEX  B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY THE SPECIAL CLEARED                                    
*                                                                               
PAYSPEC  NTR1                                                                   
         MVC   FLD+14(13),=C'CLEARED SPEC.'                                     
         MVI   FLD+30,EQUALS                                                    
*                                                                               
         SR    R0,R0                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   PAYSP80             YES                                          
         L     R4,12(R1)                                                        
         B     PAYSP20                                                          
         USING NUPAYD,R4                                                        
PAYSP10  ZIC   RE,NUPAYLEN                                                      
         AR    R4,RE                                                            
         CLI   NUPAYEL,X'12'                                                    
         BNE   PAYSP80                                                          
*                                                                               
PAYSP20  CLI   NUPAYTYP,C'T'       TEST FOR TIME                                
         BE    PAYSP10             YES                                          
         CLI   NUPAYTYP,C'I'       TEST FOR INTEGRATION                         
         BE    PAYSP10             YES                                          
*                                                                               
         ICM   RF,15,NUPAYGRS                                                   
         AR    R0,RF               ADD TO ACCUMULATOR                           
         B     PAYSP10                                                          
*                                                                               
PAYSP80  BAS   RE,EDCOS1                                                        
         BAS   RE,ADDLIN                                                        
         B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET PRODUCT CODE FROM CLIENT LIST                              
*                                                                               
GETPRD   L     R1,ACLIREC                                                       
         USING CLTHDRD,R1                                                       
         LA    R0,220                                                           
         LA    RF,CLIST                                                         
         CLC   BYTE,3(RF)          TEST FOR PRODUCT NUMBER                      
         BE    GETPR20                                                          
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
* CHECK SECOND PRODUCT TABLE                                                    
         LA    R0,35                                                            
         LA    RF,CLIST2                                                        
         CLC   BYTE,3(RF)          TEST FOR PRODUCT NUMBER                      
         BE    GETPR20                                                          
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
GETPR20  MVC   THREE,0(RF)         EXTRACT PRODUCT CODE                         
         BR    RE                  RETURN NO CALLER                             
         DROP  R1                                                               
         SPACE 2                                                                
* COMMON EDITS                                                                  
*                                                                               
EDCOS1   ST    RE,SAVEREG                                                       
         EDIT  (R0),(11,FLD+32),2,ZERO=NOBLANK,MINUS=YES                        
         B     EDX                                                              
         SPACE 2                                                                
EDCOS2   ST    RE,SAVEREG                                                       
         EDIT  (R0),(11,FLD+31),2,ZERO=NOBLANK,MINUS=YES                        
         B     EDX                                                              
         SPACE 2                                                                
EDCOS3   ST    RE,SAVEREG                                                       
         EDIT  (R0),(11,FLD+43),2,ZERO=NOBLANK,MINUS=YES                        
         B     EDX                                                              
         SPACE 2                                                                
EDX      L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PUT SAVED AREA FROM IO2                                        
*                                                                               
PUTSAVE  ST    RE,SAVEREG                                                       
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1            FIRST SAVED PAGE                             
         MVC   DMCB+10(2),TERM                                                  
         GOTO1 VDATMGR,DMCB,DMWRT,TEMPSTR,,AIOAREA2                             
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET SAVED AREA TO IO2                                          
*                                                                               
GETSAVE  ST    RE,SAVEREG                                                       
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1                                                         
         MVC   DMCB+10(2),TERM                                                  
         GOTO1 VDATMGR,DMCB,DMREAD,TEMPSTR,,AIOAREA2                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
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
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
BILHED   DC    C' DATE   TYPE  PRODUCT  NUMBER     GROSS         NET  '         
BILHED2  DC    C'   FORMAT OTHERS'                                              
PAYHED   DC    C' DATE   TYPE    REP               GROSS         NET  '         
PAYHED2  DC    C'    CHECK#   CHECK DATE'                                       
DMWRT    DC    C'DMWRT '                                                        
DMREAD   DC    C'DMREAD'                                                        
TEMPSTR  DC    C'TEMPSTR'                                                       
SORTTAB  DC    XL24'E301C302C903E404C205E206D607E708C509C10AD30BC40C'           
         DC    XL3'D80DFF'                                                      
CONVTAB  DC    XL1'01',CL4'TIME'                                                
         DC    XL1'02',CL4'BRTR'                                                
         DC    XL1'03',CL4'INT '                                                
         DC    XL1'04',CL4'CUT '                                                
         DC    XL1'05',CL4'BLAK'                                                
         DC    XL1'06',CL4'COPY'                                                
         DC    XL1'07',CL4'OTH '                                                
         DC    XL1'08',CL4'TAX '                                                
         DC    XL1'09',CL4'SECT'                                                
         DC    XL1'0A',CL4'ADM '                                                
         DC    XL1'0B',CL4'LTCH'                                                
         DC    XL1'0C',CL4'DADJ'                                                
         DC    XL1'0D',CL4'EC  '                                                
         DC    XL1'FF'                                                          
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPAYWRK                                                       
         EJECT                                                                  
* AUDIT SCREEN                                                                  
*                                                                               
         ORG   PAYLAST                                                          
       ++INCLUDE NEPAYFAD                                                       
         SPACE 2                                                                
* SCREEN EQUATES                                                                
*                                                                               
LINELEN  EQU   AUDLIN2H-AUDLIN1H                                                
LINES    EQU   (AUDLAST-AUDLIN1H)/LINELEN                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VXSORT   DS    V                                                                
*                                                                               
BILENT   DS    F                                                                
PAYENT   DS    F                                                                
*                                                                               
BILLTIME DS    F                                                                
BILLINTG DS    F                                                                
BILLSPEC DS    F                                                                
*                                                                               
MORESW   DS    C                                                                
*                                                                               
BUFFER   DS    CL150                                                            
*                                                                               
TABLE    DS    (MAXSAVE)CL(BILTABL)                                             
*                                                                               
SVKEY    DS    CL13                                                             
*                                                                               
         SPACE 2                                                                
* OVERLAY EQUATES                                                               
*                                                                               
MAXSAVE  EQU   LENSAVE/L'AUDLIN1                                                
REVERSED EQU   X'80'               BILLING STATUS EQUATES                       
REVERSAL EQU   X'40'                                                            
UNBILL   EQU   X'20'                                                            
         SPACE 2                                                                
* DSECT TO COVER BILLING TABLE ENTRIES                                          
*                                                                               
BILTABD  DSECT                                                                  
BILKEY   DS    0C                                                               
BILDAT   DS    XL2                                                              
BILTYP   DS    X                   X'01'=TIME, X'02'=INT                        
BILKEYL  EQU   *-BILKEY                                                         
BILPRD   DS    X                                                                
BILNUM   DS    CL4                                                              
BILGROSS DS    XL4                                                              
BILNET   DS    XL4                                                              
BILFORM  DS    C                   BILLING FORMAT                               
BILST    DS    X                                                                
BILTABL  EQU   *-BILTABD           LENGTH OF ENTRY                              
         SPACE 2                                                                
* DSECT TO COVER PAYING TABLE ENTRIES                                           
*                                                                               
PAYTABD  DSECT                                                                  
PAYKEY   DS    0C                                                               
PAYDAT   DS    XL2                                                              
PAYTYP   DS    X                   X'01'=TIME, X'02'=INT                        
PAYKEYL  EQU   *-PAYKEY                                                         
PAYREP   DS    CL4                                                              
PAYG     DS    XL4                                                              
PAYN     DS    XL4                                                              
PAYSEQ   DS    XL1                                                              
PAYTABL  EQU   *-PAYTABD           LENGTH OF ENTRY                              
*                                                                               
       ++INCLUDE NETBILLRD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021NEPAY06   11/20/17'                                      
         END                                                                    
