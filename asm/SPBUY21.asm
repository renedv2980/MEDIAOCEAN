*          DATA SET SPBUY21    AT LEVEL 063 AS OF 03/12/19                      
*PHASE T21121C                                                                  
         TITLE 'T21121 - SPOTPAK BUY - LINE DISPLAY II'                         
T21121   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21121,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21121+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO CLI   RCLOPT,C'D'         ESTHDR DEMO DSPLY                            
         BE    LD90                                                             
         CLI   RCLOPT,C'R'         DARE DISPLAY                                 
         BE    LDDARE                                                           
         CLI   RCLOPT,C'T'         DARE DISPLAY FOR TRADE                       
         BE    LDDARE                                                           
         CLI   RCLOPT,C'L'         DARE LIST                                    
         BE    LDDLIST                                                          
         CLI   RCLOPT,C'F'         DARE FLIGHT DISPLAY                          
         BE    LDDATES                                                          
         ZIC   RE,RCLOPT                                                        
         SLL   RE,25                                                            
         SRL   RE,23                                                            
         L     RF,RCLTAB(RE)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
* NO RETURN HERE VIA RE                                                         
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
* IF ANY UNPROTECTED FIELDS DISPLAYED, ADD TAB FIELD                            
*                                                                               
LDX      LA    R2,BUYOUTH                                                       
         BAS   RE,FNDUF                                                         
         BNE   EXIT                                                             
*                                                                               
         XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'01010000'                                          
         BAS   RE,GOBLDFLD                                                      
         B     EXIT                                                             
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         EJECT                                                                  
RCLTAB   DS    0F                                                               
         DC    AL1(0),AL3(0)                                                    
         DC    AL1(RCLROT),AL3(0)                                               
         DC    AL1(RCLPAY),AL3(0)                                               
         DC    AL1(RCLPAYDT),AL3(0)                                             
         DC    AL1(RCLINV),AL3(LD200)                                           
         DC    AL1(RCLREF),AL3(0)                                               
         DC    AL1(RCLCOM),AL3(0)                                               
         DC    AL1(RCLDEM),AL3(0)                                               
         DC    AL1(RCLACTV),AL3(0)                                              
         DC    AL1(RCLORB),AL3(0)                                               
         DC    AL1(RCLSPILL),AL3(0)                                             
         DC    AL1(RCLSTA),AL3(0)                                               
         DC    AL1(RCLHUT),AL3(0)  *DEAD*                                       
         DC    AL1(RCLPCD),AL3(0)  *DEAD*                                       
         DC    AL1(RCLINT),AL3(0)  *DEAD*                                       
         DC    AL1(RCLINTX),AL3(0) *DEAD*                                       
         DC    AL1(RCLFLM),AL3(LD200)                                           
         DC    AL1(0),AL3(0)  *DEAD*                                            
         DC    AL1(0),AL3(0)  *DEAD*                                            
         DC    AL1(RCLSCH),AL3(0)                                               
         DC    AL1(RCLCUT),AL3(0)  *DEAD*                                       
         DC    AL1(RCLRSVP),AL3(LD200)                                          
         DC    AL1(RCLDEMS),AL3(0)                                              
         DC    AL1(RCLPDEM),AL3(0)                                              
         DC    AL1(RCLPDEMX),AL3(0)                                             
         DC    AL1(RCLXCH),AL3(0)                                               
         DC    AL1(RCLDSK),AL3(LD800)                                           
         DC    AL1(RCLNET),AL3(0)                                               
         DC    AL1(RCLDT),AL3(LD820)                                            
*                                                                               
LDERR    MVI   SVRCLOPT,0                                                       
         L     R2,ABUYINPH                                                      
         GOTO1 ERROR                                                            
         EJECT                                                                  
* DISPLAY ESTHDR DEMOS                                                          
*                                                                               
LD90     CLI   SVNEWDEM,C'Y'                                                    
         BNE   LD91                                                             
         SPACE 1                                                                
* CONVERTED DEMO PROCESSING - INITIALIZE DBLOCK *                               
         SPACE 1                                                                
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
*                                                                               
         MVI   DBSELMED,C'T'                                                    
         CLI   BUYMD,C'R'                                                       
         BNE   *+12                                                             
         MVI   DBSELMED,C'R'                                                    
         B     LD91                                                             
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   LD91                                                             
         CLI   SVCXTRA,C'U'        TEST US DEMOS                                
         BE    LD91                                                             
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
*                                                                               
LD91     XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'004F2000'                                          
         LA    RE,DSPAREA                                                       
         ST    RE,BLDLIST+4                                                     
*                                                                               
         LA    R4,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         BE    *+8                                                              
         LA    R4,SVBRDEMS                                                      
         LA    R5,1                DEMO NUMBER                                  
*                                                                               
LD92     LA    R6,DSPAREA                                                       
         MVC   0(80,R6),SPACES                                                  
         LA    R7,4                DEMOS/ROW                                    
*                                                                               
LD94     CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R6),DUB                                                      
         MVI   2(R6),C'='                                                       
         CLI   SVNEWDEM,C'Y'                                                    
         BE    LD96                                                             
         MVC   3(7,R6),2(R4)       DEMO NAME                                    
         MVC   11(5,R6),=C'(123)'                                               
         ZIC   R0,0(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  12(3,R6),DUB                                                     
         LA    R4,9(R4)                                                         
         LR    RF,R4               POINT RF TO TEST MORE DEMOS                  
LD94X    LA    R5,1(R5)            REL DEMO NUMBER                              
         LA    R6,20(R6)           NEXT OUTPUT POSITION                         
         CLI   0(RF),0             TEST MORE DEMOS                              
         BE    *+8                 NO                                           
         BCT   R7,LD94                                                          
         BAS   RE,GOBLDFLD                                                      
         CLI   0(RF),0                                                          
         BNE   LD92                                                             
         B     LD98                                                             
         SPACE 1                                                                
* CONVERTED DEMO PROCESSING - GET DEMO NAME VIA DEMOCON *                       
         SPACE 1                                                                
LD96     DS    0H                                                               
         XC    DMCB(8),DMCB                                                     
         GOTO1 VCALLOV,DMCB,,X'D9000AE0'    GET A(DEMOCON)                      
         L     RF,DMCB                                                          
         LHI   RE,SVNTDMS-BUYSAVE  NON-TRAD INDEX DEMO LIST                     
         AR    RE,RA                                                            
         STCM  RE,15,DMCB+16                                                    
         GOTO1 (RF),DMCB,(R4),(2,3(R6)),(C'S',ADBLOCK),SVUSRNMS                 
*                                                                               
         LA    R4,3(R4)            NEXT DEMO                                    
         LA    RF,1(R4)            SET RF FOR E-O-L TEST                        
         B     LD94X                                                            
*                                                                               
LD98     MVI   BUYINP2H+5,0        SUPPRESS FURTHER INPUT                       
         MVC   BUYMSG(37),=C'** ESTIMATE HEADER DEMOS DISPLAYED **'             
         B     EXIT                                                             
         EJECT                                                                  
* DARE FLIGHT DISPLAY                                                           
*                                                                               
LDDATES  DS    0H                                                               
         XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'00162006'                                          
         LA    RE,DSPAREA                                                       
         ST    RE,BLDLIST+4                                                     
*                                                                               
         L     R6,ASVDARE                                                       
         USING SVDARED,R6                                                       
         LA    R4,0                                                             
         OC    SVDRFLT0,SVDRFLT0                                                
         BZ    LDDT05                                                           
         LA    R2,DSPAREA          BUILD DATA FIELD                             
         MVC   0(22,R2),SPACES                                                  
         EDIT  (R4),(2,0(R2)),ZERO=NOBLANK                                      
         GOTO1 VDATCON,DMCB,SVSTART,(11,4(R2))                                  
         MVI   12(R2),C'-'                                                      
         GOTO1 VDATCON,DMCB,(2,SVDRFLT0),(11,13(R2))                            
         BAS   RE,GOBLDFLD                                                      
*                                                                               
LDDT05   LA    R6,SVDRSTR1         FIRST FLIGHT                                 
         LA    R4,1                FLIGHT NUMBER                                
LDDT10   LA    R2,DSPAREA          BUILD DATA FIELD                             
         MVC   0(22,R2),SPACES                                                  
         OC    0(4,R6),0(R6)       ANY FLIGHT                                   
         BZ    LDDTX                                                            
         EDIT  (R4),(2,0(R2))                                                   
         GOTO1 VDATCON,DMCB,(2,0(R6)),(11,4(R2))                                
         MVI   12(R2),C'-'                                                      
         GOTO1 VDATCON,DMCB,(2,2(R6)),(11,13(R2))                               
         BAS   RE,GOBLDFLD                                                      
LDDT20   LA    R6,4(R6)            NEXT DATES                                   
         LA    R4,1(R4)            NEXT NUMBER                                  
         B     LDDT10                                                           
*                                                                               
LDDTX    MVC   BUYMSG(28),=C'** FLIGHT DATES DISPLAYED **'                      
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R6                                                               
*                                                                               
* DARE DISPLAY -  DD##                                                          
*                                                                               
LDDARE   DS    0H                                                               
         BAS   RE,SETKEY                                                        
*        CLI   DDPRD1,X'FF'        SPECIAL LIST DISPLAY                         
*        BE    LDDLIST                                                          
*                                                                               
         XC    BLDLIST,BLDLIST                                                  
         LA    RE,DSPAREA                                                       
         ST    RE,BLDLIST+4                                                     
*                                                                               
         MVI   BYTE2,0             ASSUME BOTH RECORDS FOUND!!                  
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         XI    DCKFLAG,DCKFTRDE    XOR THIS BIT                                 
         GOTO1 HIGH                                                             
         MVC   AREC,AREC3                                                       
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   LDDA05                                                           
         GOTO1 GETREC                                                           
         B     LDDA10                                                           
*                                                                               
LDDA05   OI    BYTE2,NO1REC3       RECORD 1 NOT FOUND!!                         
*                                                                               
LDDA10   MVC   KEY,KEYSAVE                                                      
         XI    DCKFLAG,DCKFTRDE    RESTORE TO ORIGINAL FLAG                     
         GOTO1 HIGH                                                             
         MVC   AREC,AREC2                                                       
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   LDDA15                                                           
         GOTO1 GETREC                                                           
         B     LDDA20                                                           
         DROP  R4                                                               
*                                                                               
LDDA15   OI    BYTE2,NO2REC2       RECORD 2 NOT FOUND!!                         
*                                                                               
LDDA20   MVC   BLDLIST(4),=X'00272001'                                          
         MVC   DSPAREA,SPACES                                                   
         MVC   DSPAREA(17),=C'DARE ORDER NUMBER'                                
         TM    BYTE2,NO1REC3       DID WE FIND FIRST RECORD!!                   
         BNZ   LDDA30                                                           
         MVC   AREC,AREC3                                                       
         L     R6,AREC                                                          
         USING DAREORDD,R6                                                      
         LA    R2,DSPAREA+30                                                    
         MVC   FULL,DOKORDER                                                    
         BAS   RE,SHWORDER                                                      
         CLI   RCLOPT,C'T'                                                      
         BE    LDDA30                                                           
         MVI   DSPAREA+38,C'T'                                                  
*                                                                               
LDDA30   TM    BYTE2,NO2REC2       DID WE FIND SECOND RECORD!!                  
         BNZ   LDDA40                                                           
         MVC   AREC,AREC2                                                       
         L     R6,AREC                                                          
         LA    R2,DSPAREA+20                                                    
         MVC   FULL,DOKORDER                                                    
         BAS   RE,SHWORDER                                                      
         CLI   RCLOPT,C'T'                                                      
         BNE   LDDA40                                                           
         MVI   DSPAREA+28,C'T'                                                  
LDDA40   BAS   RE,GOBLDFLD                                                      
*                                                                               
         MVC   BLDLIST(4),=X'00192009'                                          
         MVC   DSPAREA,SPACES                                                   
*                                                                               
         L     R8,ASVDARE                                                       
         USING SVDARED,R8                                                       
         CLI   SVDRFLAG,C'Y'       IS ESTIMATE FLIGHTED?                        
         BNE   LDD10                NO                                          
         CLI   DDFLIGHT,C'0'       DISPLAYING FLIGHT 0?                         
         BNE   LDD05                NO                                          
         MVC   DSPAREA(4),=C'FLT0'                                              
         LA    R4,DSPAREA+4                                                     
         MVI   0(R4),C'='                                                       
         GOTO1 VDATCON,DMCB,SVSTART,(11,1(R4))                                  
         MVI   9(R4),C'-'                                                       
         GOTO1 VDATCON,DMCB,(2,SVDRFLT0),(11,10(R4))                            
         B     LDD10                                                            
*                                                                               
LDD05    ZIC   R1,DDFLIGHT         DISPLAY FLIGHT DATES                         
         SH    R1,=H'1'                                                         
         BM    LDD10                                                            
         MH    R1,=H'4'            DISP TO FLIGHT DATES IN TABLE                
         L     R8,ASVDARE                                                       
         USING SVDARED,R8                                                       
         LA    R2,SVDRSTR1         START OF FLIGHTS                             
         AR    R2,R1                                                            
         OC    0(4,R2),0(R2)                                                    
         BZ    LDD10                                                            
         MVC   DSPAREA(3),=C'FLT'                                               
         LA    R4,DSPAREA+3                                                     
         EDIT  (B1,DDFLIGHT),(2,0(R4)),ALIGN=LEFT                               
         AR    R4,R0                                                            
         MVI   0(R4),C'='                                                       
         GOTO1 VDATCON,DMCB,(2,0(R2)),(11,1(R4))                                
         MVI   9(R4),C'-'                                                       
         GOTO1 VDATCON,DMCB,(2,2(R2)),(11,10(R4))                               
LDD10    BAS   RE,GOBLDFLD                                                      
*                                                                               
         MVC   BLDLIST(4),=X'00262002'                                          
         MVC   DSPAREA,SPACES                                                   
         MVC   DSPAREA(15),=C'REVISION NUMBER'                                  
         MVC   AREC,AREC3                                                       
         L     R6,AREC                                                          
         LA    R6,24(R6)           POIN TO 1ST ELEMENT                          
LDD10A05 CLI   0(R6),0             DID WE FIND IT?                              
         BE    LDD10A20            NO                                           
         CLI   0(R6),DOSPELQ       X'03'                                        
         BE    LDD10A10                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LDD10A05                                                         
         USING DOSPELD,R6                                                       
LDD10A10 LA    R4,DSPAREA+35                                                    
         EDIT  (B1,DOSPREVN),(3,(R4)),FILL=0                                    
*                                                                               
LDD10A20 MVC   AREC,AREC2                                                       
         L     R6,AREC                                                          
         LA    R6,24(R6)           POINT TO 1ST ELEMENT                         
LDD10A25 CLI   0(R6),0             DID WE FIND IT?                              
         BE    LDD20               NO                                           
         CLI   0(R6),DOSPELQ       X'03'                                        
         BE    LDD10A30                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LDD10A25                                                         
         USING DOSPELD,R6                                                       
LDD10A30 LA    R4,DSPAREA+25                                                    
         EDIT  (B1,DOSPREVN),(3,(R4)),FILL=0                                    
LDD20    BAS   RE,GOBLDFLD                                                      
*                                                                               
         MVC   BLDLIST(4),=X'00202005'                                          
         MVC   DSPAREA,SPACES                                                   
         L     R8,ASVDARE                                                       
         USING SVDARED,R8                                                       
         CLI   SVDRFLAG,C'Y'       FLIGHTS?                                     
         BNE   LDD30                                                            
*                                                                               
         MVC   DSPAREA(9),=C'FLT LIST='                                         
         LA    R4,DSPAREA+9                                                     
         OC    SVDRFLT0,SVDRFLT0   FLIGHT 0?                                    
         BZ    LDD24                                                            
         LA    R2,0                                                             
         EDIT  (R2),(2,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                           
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
*                                                                               
LDD24    LA    R8,SVDRSTR1         START OF FLIGHTS                             
         LA    R2,1                                                             
LDD25    OC    0(4,R8),0(R8)       ANY DATES                                    
         BZ    LDD30                                                            
         CH    R2,=H'1'                                                         
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         EDIT  (R2),(2,0(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R8,4(R8)            NEXT FLIGHT                                  
         LA    R2,1(R2)                                                         
         B     LDD25                                                            
LDD30    BAS   RE,GOBLDFLD                                                      
         DROP  R8                                                               
*                                                                               
         MVC   BLDLIST(4),=X'00262010'                                          
         MVC   DSPAREA,SPACES                                                   
         MVC   DSPAREA(19),=C'REP CONTRACT NUMBER'                              
         MVC   AREC,AREC3                                                       
         L     R6,AREC                                                          
         LA    R6,24(R6)           POIN TO 1ST ELEMENT                          
         CLI   0(R6),DOIDELQ       X'01'                                        
         BNE   LDD30A10                                                         
         USING DOIDELD,R6                                                       
         MVC   DSPAREA+30(L'DOIDCON),DOIDCON                                    
*                                                                               
LDD30A10 MVC   AREC,AREC2                                                       
         L     R6,AREC                                                          
         LA    R6,24(R6)           POINT TO 1ST ELEMENT                         
         CLI   0(R6),DOIDELQ       X'01'                                        
         BNE   LDD30A20                                                         
         USING DOIDELD,R6                                                       
         MVC   DSPAREA+20(L'DOIDCON),DOIDCON                                    
LDD30A20 BAS   RE,GOBLDFLD                                                      
*                                                                               
         MVI   BYTE,0              <=== CAN WE USE VARIABLE BYTE?               
         MVI   ELCDLO,DOSPELQ                                                   
         MVI   ELCDHI,DOSPELQ                                                   
         USING DOSPELD,R6                                                       
         BAS   RE,NEXTEL                                                        
         BNE   LDD31                                                            
         MVC   BYTE,DOSPREVN                                                    
         MVC   HALF(L'DOSPFLG1),DOSPFLG1                                        
*                                                                               
LDD31    L     R6,AREC                                                          
         LA    R6,24(R6)           POINT TO 1ST ELEMENT                         
         MVC   BLDLIST(4),=X'00202020'                                          
         MVC   DSPAREA,SPACES                                                   
         MVC   DSPAREA(6),=C'STATUS'                                            
         MVI   ELCDLO,DOSTELQ      X'12'                                        
         MVI   ELCDHI,DOSTELQ                                                   
         USING DOSTELD,R6                                                       
         BAS   RE,NEXTEL                                                        
         BNE   LDD65                                                            
*                                                                               
         LA    R4,DARSTTAB         STATUS TABLE                                 
LDD35    CLI   0(R4),X'FF'                                                      
         BE    LDD50                                                            
         CLC   0(1,R4),DOSTSTAT                                                 
         BE    LDD40                                                            
         LA    R4,L'DARSTTAB(R4)                                                
         B     LDD35                                                            
*                                                                               
LDD40    CLI   DOSTSTAT,QRJCT      REJECT?                                      
         BNE   LDD45                                                            
         CLI   DOSTLEN,DOSTLNQ3                                                 
         BE    LDD48                                                            
         B     LDD50                                                            
*                                                                               
LDD45    CLI   DOSTSTAT,QCFMD                                                   
         BNE   LDD50                                                            
         CLI   DOSTLEN,DOSTLNQ3    DOES IT HAVE A TYPE FIELD?                   
         BNE   LDD45A10                                                         
         TM    DOSTTYPE,DCNFMCOM   PARTIAL CONFIRM?                             
         BNZ   LDD48                                                            
         TM    DOSTTYPE,DCNFMFUL   AUTO CONFIRM?                                
         BZ    LDD50                                                            
         LA    R4,L'DARSTTAB(R4)                                                
         B     LDD48                                                            
*                                                                               
LDD45A10 TM    HALF,DOSPCFCM       SUPP ELEM X'03' PARTIAL CONFIRMED?           
         BZ    LDD50                                                            
LDD48    LA    R4,L'DARSTTAB(R4)                                                
*                                                                               
LDD50    CLI   BYTE,0                                                           
         BNE   *+14                                                             
         MVC   DSPAREA+7(24),1(R4)                                              
         B     LDD65                                                            
*                                                                               
         MVC   DSPAREA+7(4),=C'REV '                                            
         MVC   DSPAREA+7+4(20),1(R4)                                            
*                                                                               
LDD65    BAS   RE,GOBLDFLD                                                      
*                                                                               
         MVC   BLDLIST(4),=X'00322010'                                          
         MVC   DSPAREA,SPACES                                                   
         MVC   DSPAREA(4),=C'DATE'                                              
         MVC   DSPAREA+25(4),=C'TIME'                                           
         CLI   0(R6),DOSTELQ                                                    
         BNE   LDD70                                                            
         LA    R4,DOSTDATE                                                      
         LA    R5,DOSTTIME                                                      
*                                                                               
LDD68    GOTO1 VDATCON,DMCB,(8,0(R4)),(8,DSPAREA+11)                            
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,0(R5),WORK,L'DOSTTIME                                  
         MVC   DSPAREA+40(2),WORK                                               
         MVI   DSPAREA+42,C':'                                                  
         MVC   DSPAREA+43(2),WORK+2                                             
LDD70    BAS   RE,GOBLDFLD                                                      
*                                                                               
         MVC   BLDLIST(4),=X'004B2002'                                          
         MVC   DSPAREA,SPACES                                                   
         MVC   DSPAREA(5),=C'LINES'                                             
         MVC   DSPAREA+25(11),=C'REP LINE #S'                                   
         BAS   RE,GOBLDFLD                                                      
*                                                                               
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,DOBUYELQ     X'22'                                        
         MVI   ELCDHI,DOBY2ELQ     X'23'                                        
         MVI   BYTE,0              ONLY ROOM FOR 5 LINES                        
LDD75    BAS   RE,NEXTEL                                                        
         BNE   LDDX                                                             
*                                                                               
         MVC   DSPAREA,SPACES                                                   
*                                                                               
         USING DOBUYELD,R6                                                      
         CLI   0(R6),DOBUYELQ                                                   
         BNE   LDD77                                                            
         CLI   DDSTLIN,0          HAVE HIGH WATER MARK LINE#?                   
         BE    LDD76               NO                                           
         CLC   DDSTLIN,DOBUYSPT   REACHED HIGH WATER MARK LINE#?                
         BH    LDD75               NO, SKIP                                     
LDD76    EDIT  (1,DOBUYSPT),(3,DSPAREA),ALIGN=LEFT                              
         LA    R4,DOBUYREP        START OF REP LINE LIST                        
         B     LDD80                                                            
         DROP  R6                                                               
*                                                                               
         USING DOBY2ELD,R6                                                      
LDD77    CLI   0(R6),DOBY2ELQ                                                   
         JNE   *+2                                                              
         CLI   DOBY2SPT,0          GREATER THAN 255?                            
         BNE   LDD78                YES, ALWAYS DISPLAY                         
         CLI   DDSTLIN,0           HAVE HIGH WATER MARK LINE#?                  
         BE    LDD78                NO                                          
         CLC   DDSTLIN,DOBY2SPT+1  REACHED HIGH WATER MARK LINE#?               
         BH    LDD75                NO, SKIP                                    
LDD78    EDIT  (2,DOBY2SPT),(3,DSPAREA),ALIGN=LEFT                              
         LA    R4,DOBY2REP         START OF REP LINE LIST                       
         DROP  R6                                                               
*                                                                               
LDD80    LA    R5,DSPAREA+25                                                    
         LLC   R1,1(R6)                                                         
         LA    R1,0(R1,R6)         RE=A(END OF REC)                             
         SR    R1,R4               R1 WILL HAVE THE N'REPPAK LINE #'S           
         LTR   R1,R1                                                            
         BNP   LDD90                                                            
         SRL   R1,1                2 BYTES PER REP BUYLINE                      
         MVI   BYTE2,0                                                          
*                                                                               
LDD85    CLI   BYTE2,0                                                          
         BE    *+12                                                             
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         EDIT  (2,0(R4)),(4,0(R5)),ALIGN=LEFT                                   
         LA    R4,L'DOBUYREP(R4)                                                
         AR    R5,R0                                                            
         MVI   BYTE2,1                                                          
         BCT   R1,LDD85                                                         
*                                                                               
LDD90    BAS   RE,GOBLDFLD                                                      
         LLC   R1,BYTE             COUNT OF # LINES DISPLAYED                   
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         CHI   R1,5                                                             
         BL    LDD75                                                            
*                                                                               
LDDX     B     LDDRX                                                            
NO1REC3  EQU   X'80'               RECORD 1 NOT FOUND IN AREC3                  
NO2REC2  EQU   X'40'               RECORD 2 NOT FOUND IN AREC2                  
         EJECT                                                                  
*                                                                               
* DARE LIST --  DD=*                                                            
*                                                                               
LDDLIST  DS    0H                                                               
         BAS   RE,SETKEY                                                        
         XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(4),=X'00152005'                                          
         LA    RE,DSPAREA                                                       
         ST    RE,BLDLIST+4                                                     
*                                                                               
         USING LINED,R5                                                         
         LA    R5,DSPAREA                                                       
         MVC   DSPAREA(LINLNQ),SPACES                                           
         MVC   LPRD(3),=C'PRD'                                                  
         MVC   LORD(5),=C'ORDER'                                                
         USING SVDARED,R1                                                       
         L     R1,ASVDARE                                                       
         CLI   SVDRFLAG,C'Y'       USING FLIGHTS                                
         BNE   *+10                                                             
         MVC   LFLT(3),=C'FLT'                                                  
         DROP  R1                                                               
         BAS   RE,GOBLDFLD                                                      
         BAS   RE,GOBLDFLD                                                      
         BAS   RE,GOBLDFLD                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
LDDL05   CLI   DCKPRD,X'FF'                                                     
         BE    LDDRX                                                            
         ZIC   R1,DCKPRD           NEXT PRD (START AT 1)                        
         LA    R1,1(R1)                                                         
         STC   R1,DCKPRD                                                        
LDDL07   MVC   DCKEST,SVEST        SET TO GO STRAIGHT TO CORRECT EST            
         MVC   DCKSTA,SVSTA        & STATION                                    
         CLI   DCKSTA,X'E8'        IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DCKSTA+2,X'80'      YES, WE ONLY WANT SYSCODE LEVEL              
         XC    DCKPRD2(3),DCKPRD2  CLEAR REST OF KEY                            
*                                                                               
         GOTO1 HIGH                                                             
         B     LDDL20                                                           
LDDL10   LA    R6,KEY                                                           
         GOTO1 SEQ                                                              
LDDL20   CLC   KEY(DCKPRD-DOKEY),KEYSAVE  SAME AGY/MD/CLT                       
         BNE   LDDRX                                                            
         CLC   DCKEST,SVEST        ACTUAL EST TO REQ EST                        
         BH    LDDL05              IF HIGH, NEXT PRODUCT                        
         BL    LDDL07              IF LOW, READ FOR EST                         
         MVC   FULL(3),SVSTA                                                    
         CLI   FULL,X'E8'          IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    FULL+2,X'80'        YES, WE ONLY WANT SYSCODE LEVEL              
         CLC   DCKSTA,FULL         ACTUAL STA TO REQ STA                        
         BH    LDDL05              IF HIGH, NEXT PRODUCT                        
         BL    LDDL07              IF LOW, READ FOR STATION                     
*                                                                               
         CLI   DDFLIGHT,0          ANY FLIGHT TO FILTER?                        
         BE    LDDL21                                                           
         MVC   BYTE,DDFLIGHT                                                    
         CLI   DDFLIGHT,C'0'       FILTER FOR FLIGHT0?                          
         BNE   *+8                                                              
         MVI   BYTE,0               YES, SET C'0' TO BINARY-0                   
         CLC   DCKFLTNM,BYTE                                                    
         BNE   LDDL10                                                           
*                                                                               
LDDL21   LA    R5,DSPAREA                                                       
         MVC   DSPAREA(LINLNQ),SPACES                                           
         USING SVDARED,R1                                                       
         L     R1,ASVDARE                                                       
         CLI   SVDRFLAG,C'Y'       USING FLIGHTS                                
         BNE   LDDL22                                                           
         EDIT  (B1,DCKFLTNM),(2,LFLT),ZERO=NOBLANK                              
*                                                                               
LDDL22   MVC   BYTE,DCKPRD                                                      
         BAS   RE,FNDPRD                                                        
         MVC   LPRD(3),FULL                                                     
*                                                                               
         CLI   DCKPRD2,0                                                        
         BE    LDDL30                                                           
         MVC   BYTE,DCKPRD2                                                     
         BAS   RE,FNDPRD                                                        
         MVI   LPRD+3,C'-'                                                      
         MVC   LPRD+4(3),FULL                                                   
*                                                                               
LDDL30   MVC   AREC,AREC2                                                       
         GOTO1 GETREC                                                           
         L     R6,AREC                                                          
         USING DAREORDD,R6                                                      
         TM    DORSTAT,X'01'       IS IT CONFIRMED                              
         BNO   *+8                                                              
         MVI   LCNF,C'*'                                                        
         LA    R2,WORK+20          FAKE HEADER                                  
         MVC   FULL,DOKORDER                                                    
         LA    R2,LORD                                                          
         BAS   RE,SHWORDER                                                      
*                                                                               
         BAS   RE,GOBLDFLD                                                      
         BE    LDDL10                                                           
*                                                                               
LDDRX    XC    KEY,KEY                                                          
         MVI   SVLIN,0                                                          
         MVC   SVRCLOPT,RCLOPT                                                  
         MVC   BUYMSG(25),=C'** DARE INFO DISPLAYED **'                         
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
*        SET DARE ORDER KEY FOR READ HIGH                                       
*                                                                               
SETKEY   NTR1                                                                   
         USING DOKEY,R6                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   DCKTYPE,DCKTYPQ     TYPE = 0D                                    
         MVI   DCKSUBTY,DCKSTYPQ   SUB TYPE = B5                                
         MVC   DCKAGMD,SVAGYMD     AGY/MED                                      
         MVC   DCKCLT,SVCLT        CLIENT                                       
         CLI   RCLOPT,C'L'         LIST                                         
*        CLI   DDPRD1,X'FF'        LIST                                         
         BE    SKX                                                              
         MVC   DCKPRD,DDPRD1       PRODUCT                                      
         MVC   DCKPRD2,DDPRD2                                                   
         MVC   DCKEST,SVEST        ESTIMATE                                     
         MVC   DCKSTA,SVSTA        STATION                                      
         CLI   DCKSTA,X'E8'        IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DCKSTA+2,X'80'      YES, WE ONLY WANT SYSCODE LEVEL              
         CLI   DDFLIGHT,C'0'                                                    
         BE    *+10                                                             
         MVC   DCKFLTNM,DDFLIGHT   ESTIMATE FLIGHT NUMBER                       
         CLI   RCLOPT,C'T'         TRADE?                                       
         BNE   SKX                                                              
         OI    DCKFLAG,DCKFTRDE    YES                                          
SKX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        FIND CORRECT PRODUCT IN CLIST                                          
* ENTRY: BYTE - BPRD CODE                                                       
* RETURN FULL - EBCDIC PRODUCT CODE                                             
*                                                                               
FNDPRD   NTR1                                                                   
         MVC   FULL,SPACES                                                      
         L     R1,ASVCLIST         NOW LOOK UP PRD CODE IN CLIST                
*                                                                               
FP10     CLI   0(R1),0             END OF CLIST                                 
         BE    FPX                                                              
         CLC   BYTE,3(R1)                                                       
         BE    FP20                                                             
         LA    R1,4(R1)                                                         
         B     FP10                                                             
FP20     MVC   FULL(3),0(R1)       SET EBCDIC PRD CODE                          
FPX      B     EXIT                                                             
*                                                                               
* FORMAT ORDER NUMBER                                                           
* ON ENTRY:    FULL                ORDER NUMBER AS STORED IN RECORD             
* EXIT:        R2                  EBCIDIC ORDER NUMBER                         
*                                                                               
SHWORDER NTR1                                                                   
         XC    FULL,=4X'FF'                                                     
         TM    FULL,X'80'                 NEW STYLE?                            
         BNZ   SORD20                     YES                                   
*                                                                               
         L     R1,FULL                                                          
         AH    R1,=H'1'                                                         
         ZICM  R1,FULL,2                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R2),DUB                                                      
*                                                                               
         ZICM  R1,FULL+2,2                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(4,R2),DUB                SEQUENCE NUMBER                       
         B     SORDX                                                            
*                                                                               
SORD20   NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R2),DUB                                                      
SORDX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* INVOICE DATA                                                                  
*                                                                               
LD200    DS    0H                                                               
         MVI   ERRCD,RSVPPOL                                                    
         CLI   RCLOPT,RCLRSVP                                                   
         BNE   *+12                                                             
         CLI   BUYKEY+3,X'FF'      RSVP ONLY VALID FOR POL BUS                  
         BNE   BUYERR                                                           
         CLI   RCLOPT,RCLINV       TEST RECALL OPT = INVOICE                    
         BNE   LD202               NO - MUST BE RSV                             
* IF X'17' ELEMS PRESENT, NO AFFID DSPLY                                        
         MVI   ELCDLO,X'17'                                                     
         MVI   ELCDHI,X'17'                                                     
         LA    R6,BDELEM                                                        
         MVI   ERRCD,RSVPONLY                                                   
         BAS   RE,NEXTEL                                                        
         BE    LDERR                                                            
* TEST X'40' STATUS BIT (RESPONSE COUNT MATCH) IF FN NOT ACTIVE                 
         CLI   SVFNPROF,X'FF'      TEST FN PROFILE ACTIVE (FF=NO)               
         BNE   LD202               YES - NO RESPONSE COUNT DISPLAY              
         MVI   ELCDLO,X'10'                                                     
         MVI   ELCDHI,X'10'                                                     
         LA    R6,BDELEM                                                        
LD201    BAS   RE,NEXTEL                                                        
         BNE   LD202                                                            
         TM    4(R6),X'40'         TEST RESPONSE COUNT MATCH                    
         BO    LDERR                                                            
         B     LD201                                                            
*                                                                               
LD202    XC    BLDLIST,BLDLIST                                                  
         LA    R1,BLDLIST                                                       
         MVI   3(R1),1             FORCE NON-MULT OF 80 FOR 3270'S              
         LA    RE,DSPDATE                                                       
         ST    RE,4(R1)                                                         
         LA    RE,9                                                             
         TM    LDOPT,X'20'         TEST RATE O/R                                
         BZ    *+8                 NO                                           
         AH    RE,MAXCOSLN                                                      
         STC   RE,1(R1)            SET LEN                                      
         MVI   2(R1),X'20'         AND 'PROTECTED'                              
*                                                                               
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   LD204                                                            
*                                                                               
         LA    R1,8(R1)                                                         
         LA    RE,DSPPRD                                                        
         ST    RE,4(R1)                                                         
         LA    RE,3                                                             
         TM    LDOPT,X'80'         TEST P/B                                     
         BZ    *+8                                                              
         LA    RE,5(RE)                                                         
         STC   RE,1(R1)                                                         
         MVI   2(R1),X'20'         SET PROTECTED                                
*                                                                               
LD204    LA    R1,8(R1)                                                         
         LA    RE,DSPPAYDT                                                      
         ST    RE,4(R1)                                                         
         MVI   1(R1),13                                                         
         CLI   RCLOPT,RCLRSVP                                                   
         BNE   *+8                                                              
         MVI   1(R1),20                                                         
         MVI   3(R1),1             SET 1 SPACE AFTER                            
*                                                                               
         CLI   RCLOPT,RCLFLM                                                    
         BE    LD950                                                            
*                                                                               
         MVI   DSPCNTR,0                                                        
         XC    ELEMDT,ELEMDT                                                    
         LA    R6,BDELEM                                                        
*                                                                               
         MVI   ERRCD,NOELEMS                                                    
         LA    RF,POLAFD                                                        
         CLI   BUYKEY+3,X'FF'                                                   
         BE    *+8                                                              
         LA    RF,REGAFD                                                        
         BASR  RE,RF                                                            
         BNE   LDERR                                                            
*                                                                               
LD210    BAS   RE,GOBLDFLD                                                      
         BNE   LDX                                                              
         BASR  RE,RF                                                            
         BE    LD210                                                            
         B     LDX                                                              
         EJECT                                                                  
POLAFD   NTR1                                                                   
*                                                                               
         MVC   DSPAREA,SPACES                                                   
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
POLAFD2  BAS   RE,NEXTEL                                                        
         BNE   NEQXIT                                                           
*                                                                               
POLAFD2X OC    STDTP,STDTP                                                      
         BZ    POLAFD3                                                          
         CLC   STDTP,2(R6)         FILTER ON REQUESTED DATES                    
         BH    POLAFD2                                                          
         CLC   ENDDTP,2(R6)                                                     
         BL    POLAFD2                                                          
*                                                                               
POLAFD3  CLC   ELEMDT,2(R6)                                                     
         BE    *+8                                                              
         MVI   ELEMNO,0                                                         
         MVC   ELEMDT,2(R6)                                                     
         IC    RE,ELEMNO                                                        
         TM    6(R6),X'80'         TEST MINUS SPOT                              
         BO    *+8                                                              
         LA    RE,1(RE)                                                         
         STC   RE,ELEMNO                                                        
         CLC   STDTP,2(R6)                                                      
         BNE   *+14                                                             
         CLC   BUELEMNO,ELEMNO     TEST REACHED START SPOT YET                  
         BH    POLAFD2                                                          
*                                                                               
         TM    6(R6),X'04'         TEST HIATUS                                  
         BO    POLAFD2             YES - SKIP                                   
         CLI   RCLOPT,RCLINT                                                    
         BE    POLAFD3A                                                         
         CLI   RCLOPT,RCLINTX                                                   
         BE    POLAFD3A                                                         
         CLI   RCLOPT,RCLCUT       CUT-IN DISPLAYS UNALL SPOTS                  
         BE    *+12                                                             
* AFFID/FLM DISPLAY SKIPS MINUS/MINUSED/UNALL                                   
         CLI   1(R6),10            TEST UNALL                                   
         BE    POLAFD2                                                          
         TM    6(R6),X'C0'         AFFIDS SKIP MINUS/MINUSED                    
         BNZ   POLAFD2                                                          
         B     POLAFD3B            ELSE PROCESS                                 
*                                                                               
POLAFD3A DS    0H                                                               
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BZ    POLAFD3B            NO - PROCESS                                 
* DISPLAY MINUS/MINUSED ONLY IF INTG ELEM PRESENT                               
         LR    R7,R6                                                            
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'11'         TEST INTG ELEM PRESENT                       
         BNE   POLAFD2             NO - SKIP                                    
*                                                                               
POLAFD3B DS    0H                                                               
*                                                                               
         CLI   0(R6),X'0B'                                                      
         BE    *+8                                                              
         MVI   DSPDATE,C'+'                                                     
         TM    6(R6),X'80'                                                      
         BZ    *+8                                                              
         MVI   DSPDATE,C'-'                                                     
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,DSPDATE+1)                             
*                                                                               
         CLI   ELEMNO,1                                                         
         BE    POLAFD6                                                          
         MVI   DSPDATE+6,C'-'                                                   
         ZIC   R0,ELEMNO                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPDATE+7(2),DUB                                                 
         B     POLAFD6                                                          
*                                                                               
POLAFD4  DC    H'0'                                                             
*                                                                               
POLAFD6  TM    6(R6),X'20'         TEST O/R THIS ELEM                           
         BZ    POLAFD8A                                                         
         MVI   DUB,0                                                            
         MVC   DUB+1(3),7(R6)                                                   
         L     R0,DUB                                                           
         LTR   R0,R0                                                            
         BNZ   *+14                                                             
         MVC   DSPCOST(2),=C'$0'                                                
         B     POLAFD7                                                          
*                                                                               
         TM    BDSTAT,X'01'        TEST NETPAK                                  
         BO    POLAFD6B                                                         
         EDIT  (R0),(9,DSPCOST),2,ALIGN=LEFT,FLOAT=$                            
         B     POLAFD6X                                                         
POLAFD6B EDIT  (R0),(9,DSPCOST),0,ALIGN=LEFT,FLOAT=$                            
*                                                                               
POLAFD6X DS    0H                                                               
*                                                                               
         LA    RE,DSPCOST                                                       
         AR    RE,R0                                                            
         SH    RE,=H'3'                                                         
         CLC   =C'.00',0(RE)                                                    
         BNE   *+10                                                             
         MVC   0(3,RE),SPACES                                                   
*                                                                               
POLAFD7  LR    RE,R6                                                            
*                                                                               
POLAFD7A SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'10'                                                      
         BL    POLAFD8A                                                         
         CLI   0(RE),X'13'                                                      
         BL    POLAFD7A                                                         
         BH    POLAFD8A                                                         
         TM    2(RE),X'80'         TEST COST1 ONLY                              
         BZ    *+8                                                              
         MVI   DSPCOST,C'<'                                                     
         TM    2(RE),X'40'         TEST COST2 ONLY                              
         BZ    *+8                                                              
         MVI   DSPCOST,C'>'                                                     
*                                                                               
POLAFD8A LA    R0,9                                                             
         LA    RE,DSPCOST+8                                                     
POLAFD8B CLI   0(RE),C' '                                                       
         BNE   POLAFD10                                                         
         MVI   0(RE),C'.'                                                       
         BCTR  RE,0                                                             
         BCT   R0,POLAFD8B                                                      
*                                                                               
POLAFD10 CLI   1(R6),10            TEST UNALL                                   
         BE    POLAFD12                                                         
         LA    R7,10(R6)                                                        
         BAS   RE,GETCD                                                         
         MVC   DSPPRD(3),0(R1)                                                  
         CLI   1(R6),14                                                         
         BNH   POLAFD12                                                         
         LA    R7,4(R7)                                                         
         BAS   RE,GETCD                                                         
         MVI   DSPPRD+3,C'-'                                                    
         MVC   DSPPRD+4(3),0(R1)                                                
*                                                                               
POLAFD12 ZIC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   RCLOPT,RCLINV       TEST AFFID DSPLY                             
         BE    POLAFD14                                                         
         CLI   RCLOPT,RCLRSVP                                                   
         BNE   POLAFDX             NO - EXIT                                    
POLAFD14 CLI   0(R7),X'10'         TEST AFFID                                   
         BNE   POLAFD20                                                         
* DISPLAY AFFID ELEM                                                            
         LA    R4,DSPPAYDT                                                      
         CLI   SVFNPROF,X'FF'                                                   
         BE    POLAFD16                                                         
         OC    SVFNPROF,SVFNPROF                                                
         BZ    POLAFD16                                                         
* IF PROFILE FOUND MUST CHECK EST VS PROFILE START DATE                         
         CLC   DSPPRD(3),SVFNPROF+10  PROFILE APPLY TO THIS BRAND               
         BNE   POLAFD16               NO                                        
         CLC   SVENDB(3),SVFNPROF+13                                            
         BL    POLAFD16            IF EST ENDS GE PROF START USE PROF           
* DISPLAY FILM TYPE CODE                                                        
         TM    4(R7),X'F0'         IF NONE THERE SKIP IT                        
         BZ    POLAFD16                                                         
         ZIC   RE,4(R7)                                                         
         SRL   RE,4                                                             
         IC    RE,SVFNPROF-1(RE)                                                
         STC   RE,0(R4)                                                         
         MVI   1(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
*                                                                               
POLAFD16 GOTO1 VDATCON,DMCB,(2,2(R7)),(4,(R4))                                  
         MVI   5(R4),C'-'                                                       
         XC    FULL,FULL                                                        
         MVC   FULL(2),4(R7)                                                    
         NI    FULL,X'0F'          DROP FILM BITS                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'   GET UNTIME ADDRESS                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(4,FULL),6(R4)                                         
         EJECT                                                                  
POLAFD20 DS    0H                                                               
         CLI   RCLOPT,RCLRSVP                                                   
         BNE   POLAFDX                                                          
* DISPLAY RSVP ELEM                                                             
         MVC   BYTE(1),4(R7)       SAVE AFFID STATUS BYTE                       
         CLI   0(R7),X'10'         TEST AFFID PRESENT                           
         BE    POLAFD22            YES                                          
         MVI   BYTE,0              ELSE CLEAR AFFID STATUS BYTE                 
         MVC   DSPPAYDT(12),=C'**NO AFFID**'                                    
*                                                                               
POLAFD22 LA    R4,DSPPAYDT+13      FIND END OF DISPLAYED TIME                   
         OI    0(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-12                                                          
*                                                                               
POLAFD24 CLI   0(R7),X'17'         TEST RSVP ELEMENT                            
         BE    POLAFD26                                                         
         ZIC   R0,1(R7)                                                         
         AR    R7,R0               POINT TO NEXT ELEMENT                        
         CLI   0(R7),X'11'                                                      
         BL    POLAFD26            IF LOW, NO COUNT ELEM                        
         CLI   0(R7),X'1F'                                                      
         BNH   POLAFD24            IF LOW, KEEP TRYING                          
*                                                                               
POLAFD26 MVC   1(3,R4),=C', ,'                                                  
         CLI   0(R7),X'17'         TEST HAVE RSVP ELEM                          
         BNE   *+10                NO                                           
         MVC   2(1,R4),2(R7)                                                    
         LA    R4,4(R4)                                                         
*                                                                               
         SR    R0,R0                                                            
         CLI   0(R7),X'17'         TEST HAVE RSVP ELEM                          
         BNE   *+8                 NO                                           
         LH    R0,4(R7)                                                         
         EDIT  (R0),(5,(R4)),ALIGN=LEFT,ZERO=NOBLANK                            
*                                                                               
         OC    SVFNPROF,SVFNPROF   TEST FILM TYPES ACTIVE                       
         BZ    *+12                NO                                           
         CLI   SVFNPROF,X'FF'                                                   
         BNE   POLAFDX                                                          
         AR    R4,R0               POINT TO NEXT OUTPUT POSN                    
         TM    BYTE,X'40'          TEST RESPONSE OR AFFID DATA                  
         BZ    *+8                                                              
         MVI   0(R4),C'+'          INDICATE RESPONSE DATA                       
*                                                                               
POLAFDX  CR    RB,RB               SET CC EQ                                    
         XIT1  REGS=(R6)                                                        
         EJECT                                                                  
GETCD    L     R1,ASVCLIST                                                      
*                                                                               
GETCD2   CLC   0(1,R7),3(R1)                                                    
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   GETCD2                                                           
         LA    R1,=C'***'                                                       
         BR    RE                                                               
         EJECT                                                                  
REGAFD   NTR1                                                                   
*                                                                               
         MVC   DSPAREA,SPACES                                                   
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'08'                                                     
         CLI   DSPCNTR,0           TEST IN MULT DSPLY                           
         BNE   REGAFD10            YES                                          
*                                                                               
REGAFD2  BAS   RE,NEXTEL                                                        
         BNE   NEQXIT                                                           
*                                                                               
         CLC   2(2,R6),ELEMDT      COMPARE TO LAST DATE PROCESSED               
         BNH   REGAFD2             LOW OR EQ - CONTINUE                         
         OC    STDTP(4),STDTP                                                   
         BZ    REGAFD3                                                          
         CLC   STDTP,2(R6)                                                      
         BH    REGAFD2                                                          
         CLC   ENDDTP,2(R6)                                                     
         BL    REGAFD2                                                          
*                                                                               
* COUNT SPOTS THIS DATE                                                         
*                                                                               
REGAFD3  MVC   ELEMDT,2(R6)                                                     
         SR    R8,R8                                                            
         LR    R7,R6                                                            
         B     REGAFD5                                                          
*                                                                               
REGAFD4  ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    REGAFD6                                                          
         CLC   0(1,R7),ELCDHI                                                   
         BH    REGAFD6                                                          
         CLC   ELEMDT,2(R7)                                                     
         BNE   REGAFD6                                                          
REGAFD5  IC    R0,7(R7)                                                         
         TM    6(R7),X'80'                                                      
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AR    R8,R0                                                            
         B     REGAFD4                                                          
*                                                                               
REGAFD6  LTR   R8,R8               TEST ALL SPOTS MINUSED                       
         BNP   REGAFD2             YES                                          
         STC   R8,ELEMNO           ELSE SAVE NUMBER OF SPOTS                    
*                                                                               
         CLI   ELEMNO,83           MAX AFFIDS/DATE                              
         BNH   *+6                                                              
         DC    H'0'                                                             
* MOVE AFFIDS TO ELEM                                                           
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
*                                                                               
REGAFD8  CLI   0(R7),X'10'         TEST AFFID                                   
         BNE   REGAFD10                                                         
         MVC   0(4,RE),2(R7)       MOVE DATE/TIME                               
         LA    RE,4(RE)                                                         
         ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     REGAFD8                                                          
*                                                                               
REGAFD10 IC    RE,DSPCNTR                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DSPCNTR                                                       
*                                                                               
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,DSPDATE)                               
*                                                                               
         CLI   DSPCNTR,1                                                        
         BE    REGAFD12                                                         
         ZIC   R0,DSPCNTR                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   DSPDATE+5,C'-'                                                   
         UNPK  DSPDATE+6(2),DUB                                                 
*                                                                               
* TEST FOR AFFID DATA                                                           
*                                                                               
REGAFD12 ZIC   R7,DSPCNTR                                                       
         BCTR  R7,0                                                             
         SLL   R7,2                                                             
         LA    R7,ELEM(R7)                                                      
         OC    0(4,R7),0(R7)                                                    
         BZ    REGAFDX                                                          
* DISPLAY AFFID DATA                                                            
         GOTO1 VDATCON,DMCB,(2,0(R7)),(4,DSPPAYDT)                              
         MVI   DSPPAYDT+5,C'-'                                                  
         XC    FULL,FULL                                                        
         MVC   FULL(2),2(R7)       MOVE VALUE FROM TABLE                        
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'                                       
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(4,FULL),DSPPAYDT+6                                    
*                                                                               
REGAFDX  CLC   DSPCNTR,ELEMNO                                                   
         BNE   *+8                                                              
         MVI   DSPCNTR,0                                                        
         B     POLAFDX                                                          
         EJECT                                                                  
*======================================================*                        
* DAILY SCHEDULE                                       *                        
* DISPLAY FORMAT IS  DS1..(JAN12) <1234567>            *                        
*                    DS2..(JAN19) <1234567>            *                        
*======================================================*                        
         SPACE 1                                                                
LD800    DS    0H                                                               
         MVI   ERRCD,CANTSKED                                                   
         CLI   BDWKIND,C'O'                                                     
         BNE   BUYERR                                                           
         CLI   BDWKS,14                                                         
         BH    BUYERR                                                           
*                                                                               
         ZIC   R0,BDSEDAY                                                       
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         LTR   R1,R1               TEST BUY IS A ROTATOR                        
         BZ    BUYERR                                                           
         SR    R1,R0               GET NUMBER OF DAYS -1                        
         BNM   *+8                                                              
         AH    R1,=H'7'                                                         
         LR    R8,R1               SAVE ACTUAL DAYS -1 IN R8                    
*                                                                               
         LA    R1,BLDLIST                                                       
         LA    RE,DSPDATE                                                       
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'000C2001' SET 12 BYTES/PROT                           
         LA    R1,8(R1)                                                         
         LA    RE,DSPPRD                                                        
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'00070002' SET 7 BYTES/UNP                             
*                                                                               
         MVI   BUTRCODE,C'X'       SET FLAG FOR 'BUILD TABLE ONLY'              
         GOTO1 VDSKED,DMCB,(RC)                                                 
*                                                                               
         ZAP   HALF,=P'1'          SET WEEK NUMBER COUNTER                      
         ZIC   R5,BDWKS            SET NUMBER OF WEEKS TO DISPLAY               
*                                                                               
         L     R4,AREC5            POINT TO SCHEDULE DATA                       
         USING SKEDTABD,R4                                                      
*                                                                               
LD802    XC    DSPAREA,DSPAREA                                                  
         MVC   DSPDATE(2),=C'SD'   'SCHEDULE DAYS'                              
         LA    RE,DSPDATE+2                                                     
         EDIT  (P2,HALF),(2,0(RE)),ALIGN=LEFT                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(2,SKEDDATE),(4,DSPDATE+6)                          
         MVI   DSPDATE+5,C'('                                                   
         MVI   DSPDATE+11,C')'                                                  
*                                                                               
         LA    R0,7                                                             
         LA    R1,WORK                                                          
*                                                                               
LD804    MVC   0(1,R1),SKEDOLD     MOVE NUMBER OF SPOTS THIS DAY                
         MVC   7(1,R1),SKEDOLD                                                  
         OI    7(R1),X'F0'         MAKE EBCDIC                                  
         LA    R1,1(R1)                                                         
         LA    R4,SKEDNEXT                                                      
         BCT   R0,LD804                                                         
*                                                                               
         OC    WORK(7),WORK        TEST ANY SPOTS THIS WEEK                     
         BZ    LD810               NO - SKIP THIS WEEK                          
         EX    R8,*+8                                                           
         B     LD810                                                            
         MVC   DSPPRD(0),WORK+7 *EXECUTED*                                      
*                                                                               
LD810    BAS   RE,GOBLDFLD                                                      
*                                                                               
         AP    HALF,=P'1'          BUMP DISPLAY WEEK NUMBER                     
         BCT   R5,LD802            BACK FOR NEXT WEEK                           
         B     LDX                                                              
         EJECT                                                                  
*========================================================*                      
* DISPLAY LIST OF DAYS/TIMES ON EXPLODED STATIONS FOR    *                      
* CANADIAN NETWORK - STA(5)/DAY(2)/TIME(2) ARE IN REC4   *                      
*========================================================*                      
         SPACE 1                                                                
LD820    DS    0H                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                   
         MVC   BUDUB(4),0(R1)              AND SAVE IT                          
*                                                                               
         LA    R1,BLDLIST                                                       
         LA    RE,DSPDATE                                                       
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'00042001' SET 4 BYTES/PROT                            
         LA    R1,8(R1)                                                         
         LA    RE,DSPCOST                                                       
         ST    RE,4(R1)                                                         
         MVC   0(4,R1),=X'00080001' SET 8 BYTES UNP (-99/1130P)                 
*                                                                               
         L     R4,AREC4            POINT TO START OF LIST                       
*                                                                               
LD822    OC    0(5,R4),0(R4)       TEST MORE ENTRIES                            
         BZ    LDX                                                              
         GOTO1 STAPACK,DMCB,(C'U',(R4)),WORK,WORK+4                             
         MVC   DSPDATE(4),WORK+4   MOVE STATION CALL LETTERS                    
*                                                                               
         XC    DSPCOST,DSPCOST                                                  
         OC    5(2,R4),5(R4)       TEST SAME DAY AS NTWK                        
         BNZ   LD824                                                            
         CLC   BDTIMST,7(R4)       TEST SAME TIME AS NTWK                       
         BE    LD830               YES - PUT OUT SPACES                         
*                                                                               
LD824    LA    R5,DSPCOST          POINT TO OUTPUT START                        
         MVI   0(R5),C'='          ASSUME SAME DAY AS NTWK                      
         MVC   HALF,5(R4)          MOVE DAYS                                    
         LH    R0,HALF             R0 MUST BE SIGNED                            
         LTR   R0,R0                                                            
         BZ    LD826                                                            
         MVI   0(R5),C'+'                                                       
         BP    *+8                                                              
         MVI   0(R5),C'-'                                                       
         LPR   R0,R0                                                            
         EDIT  (R0),(2,1(R5)),ALIGN=LEFT                                        
         AR    R5,R0                                                            
*                                                                               
LD826    LA    R5,1(R5)            ADVANCE POINTER                              
         CLC   BDTIMST,7(R4)       TEST NEED TO DISPLAY TIME                    
         BE    LD830               NO                                           
         MVI   0(R5),C'/'          SET DAY/TIME SEPARATOR                       
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),7(R4)       SET START TIME ONLY FOR UNTIME               
         L     RF,BUDUB            GET SAVED ADDR OF UNTIME                     
         GOTO1 (RF),DMCB,WORK,1(R5)                                             
*                                                                               
LD830    BAS   RE,GOBLDFLD                                                      
         LA    R4,9(R4)            NEXT ENTRY                                   
         B     LD822                                                            
         EJECT                                                                  
* FILM NUMBERS                                                                  
*                                                                               
LD950    MVI   0(R1),2             2 LEADING SPACES MAKES SCREEN WORK           
         MVI   1(R1),15            FIELD IS 15 BYTES IF NO PB'S                 
         TM    LDOPT,X'80'         TEST ANY PB'S                                
         BZ    *+8                                                              
         MVI   1(R1),24            SET UNP FLD LEN=24                           
*                                                                               
         XC    ELEMDT,ELEMDT                                                    
         LA    R6,BDELEM                                                        
*                                                                               
         MVI   ERRCD,NOELEMS                                                    
         BAS   RE,POLAFD                                                        
         BNE   LDERR                                                            
*                                                                               
LD952    DS    0H                                                               
         LR    R7,R6                                                            
LD953    ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'10'         TEST AFFID                                   
         BE    LD953                                                            
         CLI   0(R7),X'11'         TEST INTG                                    
         BE    LD953                                                            
*                                                                               
         MVC   DUB(2),3(R7)        MOVE FILM SEQNUM                             
         CLI   0(R7),X'12'         TEST FILM ELEM                               
         BE    LD954                                                            
         CLC   =X'1808',0(R7)      TEST TRAFFIC FILM ELEM THERE                 
         BNE   LD957                                                            
* NEW CODE BELOW DISPLAYS X'18' (TRAFFIC) FILMS WITH *T* PRECEDING              
* AND MAKES FIELD PROTECTED !                                                   
         MVI   BLDLIST+16+2,X'20'  SET PROTECTED                                
         MVI   BLDLIST+19,3        SET 1 TRAILING SPACES                        
         MVC   DUB(2),2(R7)        MOVE TRAFFIC FILM SEQNUM                     
         MVC   DSPPAYDT(3),=C'*T*'                                              
         BAS   RE,GETFLM                                                        
         MVC   DSPPAYDT+3(12),WORK2                                             
         B     LD957                                                            
         SPACE 1                                                                
* DISPLAY FILM NUMBERS *                                                        
         SPACE 1                                                                
LD954    BAS   RE,GETFLM                                                        
         MVC   DSPPAYDT(12),WORK2                                               
         LA    R4,DSPPAYDT+12                                                   
*                                                                               
         CLI   1(R7),7             TEST PIGGYBACK                               
         BNE   LD955                                                            
         MVC   DUB(2),5(R7)                                                     
         BAS   RE,GETFLM                                                        
         MVI   0(R4),C'-'                                                       
         MVC   1(8,R4),WORK2                                                    
         LA    R4,9(R4)                                                         
*                                                                               
LD955    CLI   2(R7),0             TEST DAY ASSIGNED                            
         B     LD957      <======= NOP FOR GRANT'S BUG                          
         BE    LD957                                                            
         GOTO1 VCALLOV,DMCB,0,X'D9000A0F'  GET DAYUNPK ADDRESS                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),2(R7),WORK                                             
         MVI   0(R4),C'/'                                                       
         MVC   1(2,R4),WORK                                                     
*                                                                               
LD957    BAS   RE,GOBLDFLD                                                      
         BNE   LD957X                                                           
         MVI   BLDLIST+16+2,0      RESTORE FIELD TO UNPROT                      
         MVI   BLDLIST+16+1,15     FIELD IS 15 BYTES IF NO PB'S                 
         TM    LDOPT,X'80'         TEST ANY PB'S                                
         BZ    *+8                                                              
         MVI   BLDLIST+16+1,24     SET UNP FLD LEN=24                           
         MVI   BLDLIST+19,1        RESET TRAILING SPACES                        
         MVC   DSPAREA,SPACES                                                   
         BAS   RE,POLAFD                                                        
         BE    LD952                                                            
*                                                                               
LD957X   MVC   0(3,R2),=X'000101'  FORCE XMT ALL                                
         B     LDX                                                              
         SPACE 2                                                                
* SUBROUTINE TO READ NEW TRAFFIC COMMERCIAL SEQUENCE KEYS *                     
         SPACE 1                                                                
GETFLM   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AA1'                                                  
         MVC   KEY+2(3),SVKEY      A-M/CLT                                      
         MVC   KEY+6(2),DUB        NOTE SEQNUM IS KEY+5(3)                      
         OC    SVMCLCOD,SVMCLCOD   TEST SPCL TRFC CLIENT CODE                   
         BZ    *+10                                                             
         MVC   KEY+3(2),SVMCLCOD                                                
*                                                                               
         L     R8,AREC2                                                         
         CLC   KEY(13),0(R8)       TEST SAME REC AS LAST TIME                   
         BE    GETFLMX                                                          
*                                                                               
         MVC   WORK2(12),=C'************'  PRESET FOR NOT FOUND                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'TRFDIR',KEY,KEY                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                                                             
         ST    R8,AREC                                                          
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'TRFFIL',KEY+14,(R8),DMWORK           
*                                                                               
GETFLMX  XC    WORK2,WORK2                                                      
         MVC   WORK2(8),5(R8)      MOVE CMML ID TO USER AREA                    
         TM    15(R8),CMLKSTA_PCKD  X'01' - CML ALREADY PACKED?                 
         BZ    EXIT                                                             
         GOTO1 VCALLOV,DMCB,0,X'D9000AFE'  GET TRPACK ADDRESS                   
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'U',5(R8)),WORK2                                     
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* CHECK FOR FILM NUMBER PROFILE                          *                      
* FN IS A DECEASED FEATURE FOR MCNY/CC                                          
*========================================================*                      
         SPACE 1                                                                
GETFN    NTR1                                                                   
         CLI   SVFNPROF,0                                                       
         BNE   GETFNX                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0FN'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),BUYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVFNPROF,VDATAMGR                                 
         OC    SVFNPROF,SVFNPROF                                                
         BNZ   *+8                                                              
         MVI   SVFNPROF,X'FF'      INDICATE NOT FOUND                           
         CLC   SVFNPROF+13(2),=X'630C' TEST PROFILE NOT EFFECTIVE               
         BNE   *+8                                                              
         MVI   SVFNPROF,X'FF'                                                   
GETFNX   XIT1                                                                   
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
FNDUF    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    FNDUFX                                                           
         TM    1(R2),X'20'                                                      
         BO    FNDUF                                                            
         CLI   0(R2),9                                                          
         BNH   FNDUF                                                            
         CR    RB,RB                                                            
         BR    RE                                                               
FNDUFX   LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
* PROVIDE LINKAGE TO BLDFLD WITH RETURN TO ORIGINAL CALLER                      
*                                                                               
GOBLDFLD NTR1                                                                   
         GOTO1 VBLDFLD                                                          
         XIT1 REGS=(R2)                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DARSTTAB DS    0CL25                                                            
         DC    AL1(QRJCT),CL24'REJECTED'                                        
         DC    AL1(QRJCT),CL24'AMENDED'                                         
         DC    AL1(QAPP),CL24'OPENED'                                           
         DC    AL1(QCFMD),CL24'CONFIRMED'                                       
         DC    AL1(QCFMD),CL24'PARTIAL CONFIRMED'                               
         DC    AL1(QCFMD),CL24'AUTO CONFIRMED'                                  
         DC    AL1(QUNDARE),CL24'REP UNDARE'                                    
         DC    AL1(QNODARE),CL24'AGY NODARE'                                    
         DC    AL1(QERRORED),CL24'IN ERROR'                                     
         DC    AL1(QRCLCONF),CL24'RECALL/WAS CONFIRMED'                         
         DC    AL1(QRCLAPPR),CL24'RECALL/WAS OPENED'                            
         DC    AL1(QRCLDELN),CL24'RECALL/WAS DELIVERED'                         
         DC    AL1(QRCLREJD),CL24'RECALL/WAS REJECTED'                          
         DC    AL1(QRCLUNKN),CL24'RECALL/WAS UNKNOWN'                           
         DC    AL1(QRCLTRNS),CL24'RECALL/WAS XMITTED'                           
         DC    AL1(QRECALL),CL24'RECALLED/PENDING'                              
         DC    AL1(QSNTPNDG),CL24'SEND PENDING'                                 
         DC    AL1(QSNTXCNF),CL24'SEND CANCELLED, PCNF'                         
         DC    AL1(QSNTXREJ),CL24'SEND CANCELLED, RJCT'                         
         DC    AL1(QTOBESNT),CL24'TO BE SENT'                                   
         DC    AL1(QEMPTY),CL24'EMPTY'                                          
         DC    AL1(DSENT),CL24'*SENT'                                           
         DC    AL1(DDLVRD),CL24'SENT'                                           
         DC    AL1(DFXSENT),CL24'FAX SENT'                                      
         DC    AL1(DFXDLVD),CL24'FAX DELIVERED'       X'12' TYPE                
         DC    AL1(QFAXDLVD),CL24'FAX DELIVERED'      X'11' TYPE                
         DC    AL1(QFAXCNCL),CL24'FAX ERROR'                                    
         DC    AL1(DEMSENT),CL24'EMAIL SENT'                                    
         DC    AL1(DEMDLVD),CL24'EMAIL DELIVERED'                               
         DC    AL1(QBYRCNFM),CL24'BUYER CONFIRMED'                              
         DC    X'FF',CL24'UNKNOWN   '                                           
*                                                                               
* DEDBLOCK                                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
LINED    DSECT                                                                  
LPRD     DS    CL7                 PRODUCT PAIR                                 
         DS    CL1                                                              
LORD     DS    CL8                                                              
LCNF     DS    CL1                                                              
         DS    CL1                                                              
LFLT     DS    CL2                 FLIGHT                                       
         DS    CL1                                                              
LINLNQ   EQU   *-LPRD                                                           
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063SPBUY21   03/12/19'                                      
         END                                                                    
