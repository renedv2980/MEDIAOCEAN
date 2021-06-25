*          DATA SET NEBUY24    AT LEVEL 010 AS OF 12/13/10                      
*PHASE T31124A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - AUDIT TRAIL OVERLAY - T31124'              
T31124   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AUDT**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
         SPACE 2                                                                
* EDIT ACTION FIELD                                                             
*                                                                               
AU       LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         TM    MODE,FIRST                                                       
         BZ    *+8                                                              
         OI    MODE,DISPLAY        FIRST TIME MEANS START DISPLAY               
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,X'FF'                                                      
         BE    AU3                                                              
         SPACE                                                                  
AU2      XC    FTERM,FTERM                                                      
         MVI   FTERM,DASH                                                       
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BNE   AU4                                                              
*                                                                               
AU3      MVI   FERN,MISERR                                                      
         MVC   XTRA(9),=C'UNIT DATE'                                            
         B     ERROR                                                            
         SPACE                                                                  
AU4      MVI   FERN,DATERR                                                      
         GOTO1 VDATVAL,DMCB,(1,FLD),DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         CLC   FLDH+5(1),3(R1)     TEST IF DATE MAKES UP WHOLE FIELD            
         BNE   ERROR               NO                                           
         MVC   DUB(2),ESTSTART     SET YEAR OF UNIT DATE                        
         CLC   ESTSTART(2),ESTEND                                               
         BE    AU5                                                              
         CLC   DUB+2(4),ESTSTART+2                                              
         BNL   *+10                                                             
         MVC   DUB(2),ESTEND                                                    
         SPACE                                                                  
AU5      GOTO1 VDATCON,(R1),DUB,(2,DATE)                                        
         MVC   CHARDATE,DUB                                                     
         MVI   SUB,1               DEFAULT SUB LINE IS 1                        
         CLI   FSTOP,DASH          TEST IF DASH FOUND                           
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
AU10     CLC   THISVALS,SVVALS                                                  
         BE    *+8                                                              
         OI    MODE,DISPLAY                                                     
         MVC   SVVALS(SVVALSLN),THISVALS                                        
         GOTO1 VCLEARF,DMCB,(1,AUDLIN1H),AUDLAST                                
         TM    MODE,DISPLAY                                                     
         BO    AU12                                                             
         BAS   RE,GETSAVE                                                       
         B     AU20                                                             
         SPACE                                                                  
AU12     BAS   RE,GETUNIT          GET THE UNIT                                 
         MVC   DMCB+4(4),=X'D9000A12'  GET A(XSORT)                             
         GOTO1 VCALLOV,DMCB,0                                                   
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
         MVC   BUYMSG(27),=C'AUDIT TRAIL DATA DISPLAYED.'                       
         MVC   BUYMSG+29(20),=C'PRESS ENTER FOR MORE'                           
         CLI   MORESW,YES                                                       
         BE    AU35                                                             
         MVC   BUYMSG+29(20),=CL20'ENTER NEXT REQUEST'                          
         XC    SVDATA(SVDATAL),SVDATA                                           
         B     AU35                                                             
         SPACE                                                                  
AU35     LA    R2,BUYACTH                                                       
         OI    6(R2),X'01'                                                      
         ST    R2,FADDR                                                         
         NI    MODE,X'FF'-FIRST-DISPLAY                                         
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO GET UNIT RECORD                                                
*                                                                               
GETUNIT  ST    RE,SAVEREG                                                       
         L     RE,APACKREC                                                      
         MVC   NBSELDP,NPAKDP-NPRECD(RE)                                        
         MVC   NBSELSTR,CHARDATE                                                
         MVC   NBSELEND,NBSELSTR                                                
         MVC   NBSELPRG,PROG                                                    
         MVC   NBSELSUB,SUB                                                     
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBDIRECT,YES                                                     
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBUSER+13,NO        **FUDGE PROFILE TO RETURN PRE-EMPTS          
         MVC   NBAIO,AIOAREA1                                                   
         MVI   FERN,NOTFOUND                                                    
         SPACE                                                                  
GETUNIT2 GOTO1 VNETIO,DMCB,NEBLOCKD                                             
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
* SUB-ROUTINE TO FORMAT BILLING DETAILS                                         
*                                                                               
FBILL    NTR1                                                                   
         MVC   FLD+24(21),=C'** BILLING DETAILS **'                             
         CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   FBILLA              NO                                           
         MVC   FLD+47(3),=C'DA='                                                
         GOTO1 VHEXOUT,DMCB,NBKEY+NDIRDA,FLD+50,NDIRLDA,0                       
*                                                                               
FBILLA   BAS   RE,ADDLIN                                                        
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
         CLI   NBPRD,0                                                          
         BE    FBILL2                                                           
         MVC   FLD+14(10),=C'ALLOCATION'                                        
         MVC   BYTE,NBPRD                                                       
         BAS   RE,GETPRD                                                        
         MVI   FLD+30,EQUALS                                                    
         MVC   FLD+32(3),THREE                                                  
         CLI   NBPRD2,0            TEST FOR SECOND PRODUCT                      
         BNE   FBILL1              YES                                          
         BAS   RE,ADDLIN           NO-WRAP UP FOR ONE BRAND ONLY                
         B     FBILL2                                                           
         SPACE 1                                                                
FBILL1   MVC   BYTE,NBPRD2                                                      
         BAS   RE,GETPRD                                                        
         LA    R1,FLD+34                                                        
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),COMMA                                                      
         LA    R1,1(R1)                                                         
         MVC   0(3,R1),THREE                                                    
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
         L     R0,NBBILTGR                                                      
         BAS   RE,EDCOS1                                                        
         MVI   FLD+30,EQUALS                                                    
         BAS   RE,ADDLIN                                                        
*                                                                               
         MVC   FLD+14(11),=C'BILLED INT.'                                       
         L     R0,NBBILIGR                                                      
         BAS   RE,EDCOS1                                                        
         MVI   FLD+30,EQUALS                                                    
         BAS   RE,ADDLIN                                                        
         SPACE                                                                  
FBILL4   XC    BILENT,BILENT       ZERO COUNT OF BILLING ENTRIES                
         L     R1,NBAIO                                                         
         LA    R1,NUMAINEL-NUKEY(R1)                                            
         SR    R0,R0                                                            
         LA    R4,TABLE            R4 POINTS TO TABLE OF BILLING DATA           
         USING BILTABD,R4                                                       
         SPACE                                                                  
FBILL5   CLI   0(R1),0             TEST FOR EOR                                 
         BE    FBILL8                                                           
         CLI   0(R1),X'10'         TEST FOR BILLING ELEMENT                     
         BNE   FBILL6                                                           
         USING NUBILD,R1                                                        
         TM    NUBILST,UNBILLED    TEST FOR UNBILLED ELEMENT                    
         BO    FBILL6              YES-DO NOT DISPLAY IT                        
         XC    BILTABD(BILTABL),BILTABD CLEAR NEXT ENTRY                        
         MVC   BILDAT,NUBILDAT                                                  
         MVI   BILTYP,X'01'                                                     
         CLI   NUBILTYP,C'T'                                                    
         BE    *+8                                                              
         MVI   BILTYP,X'02'                                                     
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
         SPACE                                                                  
FBILL6   IC    R0,1(R1)            NEXT ELEMENT                                 
         AR    R1,R0                                                            
         B     FBILL5                                                           
         SPACE                                                                  
FBILL8   ICM   R2,15,BILENT        SORT THE TABLE                               
         BZ    FBILLX              NO BILLING ELEMENTS                          
         GOTO1 VXSORT,DMCB,TABLE,(R2),BILTABL,BILKEYL,BILKEY-BILTABD            
         LA    R4,TABLE            POINT TO FIRST ENTRY FOR DISPLAY             
         BAS   RE,ADDLIN                                                        
         MVC   FLD(L'BILHED),BILHED SET BILLING HEADLINE                        
         MVC   FLD+L'BILHED(L'BILHED2),BILHED2                                  
         BAS   RE,ADDLIN                                                        
         SPACE                                                                  
FBILL10  GOTO1 VDATCON,DMCB,(2,BILDAT),(4,FLD+1)                                
         MVC   FLD+8(4),=C'TIME'                                                
         CLI   BILTYP,X'01'                                                     
         BE    *+10                                                             
         MVC   FLD+8(4),=C'INT '                                                
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
         MVI   PAYTYP,X'01'                                                     
         CLI   NUPAYTYP,C'I'                                                    
         BNE   *+8                                                              
         MVI   PAYTYP,X'02'                                                     
         CLI   NUPAYTYP,C'C'                                                    
         BNE   *+8                                                              
         MVI   PAYTYP,X'03'                                                     
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
FPAY5    MVC   PAYGROSS(8),NUPAYGRS                                             
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
         BAS   RE,ADDLIN                                                        
         SPACE                                                                  
FPAY10   GOTO1 VDATCON,DMCB,(2,PAYDAT),(4,FLD+1)                                
         MVC   FLD+8(4),=C'TIME'                                                
         CLI   PAYTYP,X'02'                                                     
         BNE   *+10                                                             
         MVC   FLD+8(4),=C'INT '                                                
         CLI   PAYTYP,X'03'                                                     
         BNE   *+10                                                             
         MVC   FLD+8(4),=C'BRTR'                                                
         MVC   FLD+16(4),PAYREP                                                 
         ICM   R0,15,PAYGROSS                                                   
         BAS   RE,EDCOS2                                                        
         ICM   R0,15,PAYNET                                                     
         BAS   RE,EDCOS3                                                        
         SPACE                                                                  
FPAY12   BAS   RE,ADDLIN                                                        
         LA    R4,PAYTABL(R4)                                                   
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
* SUB-ROUTINE TO GET PRODUCT CODE FROM CLIENT LIST                              
*                                                                               
GETPRD   LA    R0,255                                                           
         LA    RF,CLILIST                                                       
         CLC   BYTE,3(RF)          TEST FOR PRODUCT NUMBER                      
         BE    *+14                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   THREE,0(RF)         EXTRACT PRODUCT CODE                         
         BR    RE                  RETURN NO CALLER                             
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
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,,AIOAREA2                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET SAVED AREA TO IO2                                          
*                                                                               
GETSAVE  ST    RE,SAVEREG                                                       
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,1                                                         
         MVC   DMCB+10(2),TERM                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,AIOAREA2                           
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
DMWRT    DC    C'DMWRT '                                                        
DMREAD   DC    C'DMREAD'                                                        
TEMPSTR  DC    C'TEMPSTR'                                                       
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* AUDIT SCREEN                                                                  
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF4D                                                       
         SPACE 2                                                                
* SCREEN EQUATES                                                                
*                                                                               
LINELEN  EQU   AUDLIN2H-AUDLIN1H                                                
LINES    EQU   (AUDLAST-AUDLIN1H)/LINELEN                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    0C                                                               
*                                                                               
SVVALS   DS    0C                                                               
SVDATE   DS    XL2                 SAVED AIR DATE                               
SVSUB    DS    X                   SAVED SUB-LINE                               
SVVALSLN EQU   *-SVVALS                                                         
*                                                                               
SVTABNTR DS    X                   NUMBER OF TABLE ENTRIES                      
SVLNTRY  DS    X                   NUMBER OF LAST ENTRY DISPLAYED               
SVDATAL  EQU   *-SVDATA                                                         
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
*!!!VXSORT   DS    V                                                            
*                                                                               
BILENT   DS    F                                                                
PAYENT   DS    F                                                                
*                                                                               
THISVALS DS    0CL(SVVALSLN)                                                    
DATE     DS    XL2                 DATE EXTRACTED FROM ACTION FIELD             
SUB      DS    X                   SUBLINE EXTRACTED FROM ACTION FIELD          
CHARDATE DS    CL6                 DATE FROM ACTION FIELD - YYMMDD              
*                                                                               
MORESW   DS    C                                                                
*                                                                               
TABLE    DS    (MAXSAVE)CL(BILTABL)                                             
         SPACE 2                                                                
* OVERLAY EQUATES                                                               
*                                                                               
EQUALS   EQU   C'='                                                             
LENTWA   EQU   2304                                                             
LENSAVE  EQU   6144                                                             
MAXSAVE  EQU   LENSAVE/L'AUDLIN1                                                
REVERSED EQU   X'80'               BILLING STATUS EQUATES                       
REVERSAL EQU   X'40'                                                            
UNBILLED EQU   X'20'                                                            
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
PAYGROSS DS    XL4                                                              
PAYNET   DS    XL4                                                              
PAYTABL  EQU   *-PAYTABD           LENGTH OF ENTRY                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010NEBUY24   12/13/10'                                      
         END                                                                    
