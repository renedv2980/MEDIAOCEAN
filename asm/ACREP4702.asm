*          DATA SET ACREP4702  AT LEVEL 021 AS OF 05/01/02                      
*PHASE AC4702A                                                                  
         TITLE 'RETAIL DISTRIBUTION'                                            
AC4702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC47**,RR=R5                                                 
         USING AC4702+4096,R9                                                   
         USING AC4702D,RC                                                       
         USING ACWORKD,RA                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         L     RA,0(R1)                                                         
         LA    RC,SPACEND                                                       
         ST    R5,RELO                                                          
         SPACE 1                                                                
         EJECT                                                                  
*              SELECT APPROPRIATE MODE                                          
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RD100                                                            
         CLI   MODE,REQFRST                                                     
         BE    RD200                                                            
         CLI   MODE,LEVAFRST                                                    
         BE    RD300                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    RD400                                                            
         CLI   MODE,LEVALAST                                                    
         BE    RD500                                                            
         CLI   MODE,LEVBLAST                                                    
         BE    RD600                                                            
         CLI   MODE,ACCLAST                                                     
         BE    RD700                                                            
         CLI   MODE,PROCACC                                                     
         BE    RD800                                                            
         CLI   MODE,PROCTRNS                                                    
         BE    RD900                                                            
         CLI   MODE,REQLAST                                                     
         BE    RDA00                                                            
         CLI   MODE,RUNLAST                                                     
         BE    RDB00                                                            
         CLI   MODE,LEDGFRST                                                    
         BE    RDE00                                                            
         SPACE 3                                                                
RDEXT    XMOD1 1                                                                
         EJECT                                                                  
*              RUNFRST                                                          
         SPACE 2                                                                
RD100    XC    ID,ID               SET UP WORKER KEY                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'A47'                                                  
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         ZAP   TOTCNT,=PL6'0'           INITIALIZE TOTAL COUNT                  
         ZAP   TOTCSH,=PL6'0'           INITIALIZE TOTAL CASH                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         SPACE 1                                                                
         B     RDEXT                                                            
         EJECT                                                                  
*              REQFRST                                                          
         SPACE 2                                                                
RD200    MVI   RCSUBPRG,0                                                       
         LA    R5,SUMBUK                                                        
         LA    R6,8                                                             
         ZAP   0(6,R5),=P'0'                                                    
         LA    R5,6(R5)                                                         
         BCT   R6,*-10                                                          
         LA    R6,ADVDIS                                                        
         LA    R7,4                                                             
         ZAP   0(6,R6),=P'0'                                                    
         LA    R6,6(R6)                                                         
         BCT   R7,*-10                                                          
         MVC   PAGE,=H'1'                                                       
         MVI   FCRDTRNS,C'Y'                                                    
         SPACE 2                                                                
         LA    R6,MEDCTB           MEDIA CODE TABLE                             
         LA    R7,15                                                            
         XC    0(17,R6),0(R6)                                                   
         LA    R6,17(R6)                                                        
         BCT   R7,*-10                                                          
         XC    CNT,CNT                                                          
         MVC   SCHCD,QOPT1         SCHEME CODE                                  
         MVC   SVACC(5),=C'FIRST'                                               
         SPACE 1                                                                
         OPEN  (RETLWRK,(OUTPUT))                                               
         XC    START,START                                                      
         XC    END,END                                                          
         CLC   QSTART,SPACES                                                    
         BE    RDEXT                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
         MVI   QOPT3,C' '          FORCE IT TO DRAFT                            
         CLI   QOPT4,C'Y'                                                       
         BNE   EXITPGM                                                          
         LA    R7,RETTAPE                                                       
         GOTO1 DYNALLOC,DMCB,DDPARM,DSPARM                                      
         OPEN  ((R7),(OUTPUT))                                                  
         XC    DISTTAB,DISTTAB                INITIALIZE TABLE                  
EXITPGM  B     RDEXT                                                            
         EJECT                                                                  
*              LEDGFRST                                                         
         SPACE 2                                                                
         USING ACHEIRD,RF                                                       
RDE00    BAS   RE,LVAFND                                                        
         STM   R3,R4,LEVAD           LEVEL A DISP AND LENGTH                    
         BAS   RE,LVBFND                                                        
         STM   R3,R4,LEVBD                                                      
         BAS   RE,LVCFND                                                        
         STM   R3,R4,LEVCD                                                      
         B     RDEXT                                                            
         SPACE 2                                                                
*                                                                               
LVAFND   L     RF,ADLDGHIR         LEVEL A START IN R3                          
         LA    R3,3                                                             
         XR    R4,R4                                                            
         IC    R4,ACHRLEVA                                                      
         BCTR  R4,0                                                             
         BR    RE                                                               
*                                                                               
LVBFND   L     RF,ADLDGHIR         LEVEL B START IN R3                          
         XR    R3,R3                                                            
         XR    R5,R5                                                            
         IC    R5,ACHRLEVA                                                      
         LA    R3,3(R5,R3)                                                      
         XR    R4,R4                                                            
         IC    R4,ACHRLEVB                                                      
         SR    R4,R5                                                            
         BCTR  R4,0                                                             
         BR    RE                                                               
*                                                                               
LVCFND   L     RF,ADLDGHIR                                                      
         XR    R3,R3                                                            
         XR    R5,R5                                                            
         IC    R5,ACHRLEVB                                                      
         LA    R3,3(R5,R3)                                                      
         XR    R4,R4                                                            
         IC    R4,ACHRLEVC                                                      
         SR    R4,R5                                                            
         BCTR  R4,0                                                             
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*              LEVAFRST                                                         
         SPACE 2                                                                
         USING ACDISTD,R3                                                       
RD300    L     R3,ADHEIRA                                                       
         CLI   3(R3),C'*'     DUMMY                                             
         BE    RDEXT                                                            
         MVC   SVACC(5),=C'FIRST'                                               
         BAS   R6,GET62       GET 62 ELEMENT FOR SCHRME                         
         B     RDEXT               NOT FOUND                                    
         ZAP   LEVATOT,ACDIVAL     TOTAL UNITS FOR LEVEL A                      
         MVI   FCRDTRNS,C'N'       NO MORE TRANSACTIONS                         
         SPACE 1                                                                
         MVC   SUMTB,SPACES                                                     
         L     R3,ADHEIRA                                                       
         MVC   SUMKY,3(R3)         MOVE REGION NUMBER AND NAME TO WORK          
         L     R3,ADLVANAM                                                      
         SR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SUMNM(0),2(R3)                                                   
         PUT   RETLWRK,SUMTB                                                    
         B     RDEXT                                                            
         EJECT                                                                  
*              LEVBFRST                                                         
         SPACE 2                                                                
RD400    LA    R5,SUMBUK                                                        
         LA    R6,8                                                             
         ZAP   0(6,R5),=P'0'                                                    
         LA    R5,6(R5)                                                         
         BCT   R6,*-10                                                          
         L     R3,ADHEIRB                                                       
         CLI   3(R3),C'*'          DUMMY                                        
         BE    RDEXT                                                            
         MVI   FORCEHED,C'Y'       NEW PAGE FOR MARKET                          
         MVC   SVACC(5),=C'FIRST'                                               
         ZAP   LEVBTOT,=P'0'                                                    
         BAS   R6,GET62            GET 62 ELEMENT FOR SCHEME                    
         B     RDEXT               NOT FOUND                                    
         ZAP   LEVBTOT,ACDIVAL     TOTAL UNITS FOR LEVEL B                      
         SPACE 1                                                                
         MVC   SUMTB,SPACES                                                     
         L     R3,ADHEIRB          MARKET NUMBER AND NAME TO WORK               
         MVC   SUMKY,3(R3)                                                      
         L     R3,ADLVBNAM                                                      
         SR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SUMNM(0),2(R3)                                                   
         B     RDEXT                                                            
*                                                                               
LVSPA    CLC   0(0,R3),SPACES                                                   
MVSTCD   MVC   P+2(0),0(R3)                                                     
         EJECT                                                                  
*              LEVALAST                                                         
         SPACE 2                                                                
RD500    L     R3,ADHEIRA                                                       
         CLI   3(R3),C'*'                                                       
         BE    RDEXT                                                            
         CP    LEVADIS,=P'0'                                                    
         BE    RD510                                                            
         BAS   RE,SPAC2            SKIP LINE                                    
         MVC   P+76(16),=C'TOTAL FOR REGION'                                    
         LA    R6,LEVADIS                                                       
         BAS   R7,PRTOT                                                         
         BAS   RE,REPRT            PRINT                                        
         SPACE 1                                                                
         MVI   SUMTB+1,X'FF'                                                    
         MVC   SUMTB+2(13),SUMTB+1                                              
         PUT   RETLWRK,SUMTB                                                    
         SPACE 1                                                                
RD510    AP    ADVDIS,LEVADIS                                                   
         SP    LEVADIS,LEVADIS                                                  
         SP    LEVBDIS,LEVBDIS                                                  
         SP    LEVCDIS,LEVCDIS                                                  
         B     RDEXT                                                            
         EJECT                                                                  
*              LEVBLAST                                                         
         SPACE 2                                                                
RD600    L     R3,ADHEIRB                                                       
         CLI   3(R3),C'*'                                                       
         BE    RDEXT                                                            
         CP    LEVCDIS,=P'0'                                                    
         BE    RD610                                                            
         BAS   RE,STORTOT          DO STORE TOTAL                               
         BAS   RE,SPAC2            SKIP LINE                                    
RD610    CP    LEVBDIS,=P'0'                                                    
         BE    RD620                                                            
         MVC   P+76(16),=C'TOTAL FOR MARKET'                                    
         LA    R6,LEVBDIS                                                       
         BAS   R7,PRTOT                                                         
         BAS   RE,REPRT                                                         
         PUT   RETLWRK,SUMTB                                                    
RD620    AP    LEVADIS,LEVBDIS                                                  
         SP    LEVBDIS,LEVBDIS                                                  
         SP    LEVCDIS,LEVCDIS                                                  
         B     RDEXT                                                            
         EJECT                                                                  
*              ACCLAST                                                          
         SPACE 2                                                                
         USING BLTBD,R7                                                         
RD700    L     R3,ADACC                                                         
         A     R3,LEVCD                                                         
         CLI   0(R3),C'*'                                                       
         BNE   RDEXT                                                            
*                                  PRINT ALL UNDISTRIBUTED BILLS                
         L     R7,=V(BILLTAB)                                                   
         A     R7,RELO                                                          
         L     R8,CNT                                                           
         LTR   R8,R8                                                            
         BZ    RDEXT                                                            
RD710    LR    R6,R7                                                            
         S     R6,=AL4(BLEND-BLSTR)                                             
         CLC   0(2,R7),0(R6)       CHANGE MEDIA                                 
         BE    RD712               NO CHANGE                                    
               SPACE 1                                                          
         C     R8,CNT              FIRST TIME                                   
         BE RD711                                                               
         BAS   RE,MEDTOT           MEDIA TOTAL                                  
RD711    MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
RD712    LA    R3,3(R7)            REGION/MARKET IN TABLE                       
         L     R5,LEVAL            GET LENGTH OF REG/MARK                       
         A     R5,LEVBL                                                         
         LA    R5,1(R5)                                                         
         EX    R5,LVSPA            REGION/MARKET SPACES                         
         BE    RD770               NATIONAL                                     
         SPACE 1                                                                
         S     R5,LEVAL            CHECK MARKET                                 
         BCTR  R5,0                                                             
         A     R3,LEVAL                                                         
         LA    R3,1(R3)                                                         
         EX    R5,LVSPA            MARKET SPACES                                
         BE    RD730               MUST BE A REGION                             
         SPACE 1                                                                
         LA    R3,3(R7)                                                         
         L     R5,LEVAL            GET R3 TO REG/MARKET                         
         A     R5,LEVBL            R5,TO LENGTH                                 
         LA    R5,1(R5)                                                         
         EX    R5,CHANG            CHANGE IN REG/MARKET                         
         BE    RD780                                                            
         BAS   RE,SPAC2                                                         
         EX    R5,MVNAME                                                        
         MVC   P+1(16),=C'BILLS FOR MARKET'                                     
         B     RD780                                                            
         SPACE 2                                                                
RD730    LA    R3,3(R7)            R3 TO REGION                                 
         L     R5,LEVAL            R5, TO LENGTH                                
         EX    R5,CHANG                                                         
         BE    RD780                                                            
         BAS   RE,SPAC2                                                         
         EX    R5,MVNAME                                                        
         MVC   P+1(16),=C'BILLS FOR REGION'                                     
         B     RD780                                                            
         SPACE 2                                                                
RD770    CLC   0(2,R7),0(R6)       NEW MEDIA                                    
         BE    RD780                                                            
         MVC   P+1(14),=C'NATIONAL BILLS'                                       
         SPACE 1                                                                
RD780    BAS   RE,DETAIL                                                        
         AP    ADVDIS,BLTOT                                                     
         AP    LEVCDIS,BLTOT                                                    
         B     RD790                                                            
         SPACE 2                                                                
RD790    BAS   RE,REPRT            PRINT                                        
         A     R7,=AL4(BLEND-BLSTR)                                             
         BCT   R8,RD710                                                         
         SPACE 1                                                                
         BAS   RE,MEDTOT           MEDIA TOTAL                                  
         BAS   RE,SPAC2            SKIP LINE                                    
         MVC   P+60(16),=C'ADVERTISER TOTAL'                                    
         LA    R2,P+77                                                          
         EDIT  ADVDIS,(11,0(R2)),2,MINUS=YES                                    
         BAS   RE,REPRT            PRINT                                        
         LA    R6,ADVDIS                                                        
         LA    R7,4                                                             
         SP    0(6,R6),0(6,R6)                                                  
         LA    R6,6(R6)                                                         
         BCT   R7,*-10                                                          
         MVC   PAGE,=H'1'                                                       
         B     RDEXT                                                            
         SPACE 2                                                                
CHANG    CLC   3(0,R7),3(R6)                                                    
MVNAME   MVC   P+19(0),0(R3)                                                    
         EJECT                                                                  
*              PROCACC                                                          
         SPACE 2                                                                
         USING ACDISTD,R3                                                       
         USING PSHEADD,R6                                                       
RD800    L     R3,ADACC                                                         
         A     R3,LEVCD                                                         
         CLI   0(R3),C'*'          DUMMY ADVERTISER                             
         BNE   RD810                                                            
         L     R3,ADACC                                                         
         BAS   R6,GET62          62 WITH MATCHING SCHEME                        
         B     RDEXT             NOT FOUND                                      
         ZAP   ADVTOT,ACDIVAL      NATIONAL TOTAL                               
         B     RDEXT                                                            
         SPACE 2                                                                
RD810    L     R5,LEVCL            LEVEL C LENGTH                               
         EX    R5,LVSPA            IS IT SPACES                                 
         BE    RDEXT               NOT A STORE -BYPASS                          
         L     R3,ADACC                                                         
         BAS   R6,GET62            GET 62 WITH MACHING SCHEME                   
         B     RDEXT               NOT FOUND- BYPASS                            
         SPACE 2                                                                
         MVI   RCSUBPRG,1                                                       
         ST    R3,SV3              SAVE ADDRESS OF 62 ELEMENT                   
         CLC   SVACC(5),=C'FIRST'                                               
         BE    RD813                                                            
         L     R3,ADACC                                                         
         CLC   0(15,R3),SVACC                                                   
         BE    RD820                                                            
         CP    LEVCDIS,=P'0'                                                    
         BE    RD813                                                            
         SPACE 1                                                                
         BAS   RE,STORTOT                                                       
         SPACE 2                                                                
RD813    L     R3,ADACC                                                         
         A     R3,LEVCD                                                         
         L     R5,LEVCL                                                         
         EX    R5,MVSTCD           MOVE STORE CODE                              
         SPACE 1                                                                
         L     R3,ADLVCNAM                                                      
         XR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         SH    R4,=H'2'                                                         
         LA    R5,P+8                                                           
         LA    R3,2(R3)                                                         
         GOTO1 CHOPPER,DMCB,((R4),(R3)),(21,(R5)),(C'P',2)                      
         SPACE 1                                                                
         L     R3,ADACC                                                         
         MVC   SVACC,0(R3)                                                      
         SPACE 2                                                                
RD820    L     R8,CNT              NUMBER OF BILLS IN TABLE                     
         LTR   R8,R8                                                            
         BZ    RDEXT                                                            
         L     R7,=V(BILLTAB)                                                   
         A     R7,RELO                                                          
RD825    BAS   RE,DISTRIB          DISTRIBUTER BILL                             
         AP    LEVCDIS,POSTAMT                                                  
         BAS   RE,PRTBDET          PRINT DISB. BILL LINE                        
         BAS   RE,GENWRK      GENERATE WORKER RECORDS                           
         SPACE 1                                                                
         BAS   RE,ADDSUM                                                        
RD827    A     R7,=AL4(BLEND-BLSTR)                                             
         BCT   R8,RD825                                                         
         B     RDEXT                                                            
         EJECT                                                                  
DISTRIB  LA    R6,ADVTOT           TOTAL ADVERTISER UNITS                       
         LA    R3,3(R7)            R3 TO LEVELS IN TABLE                        
         LA    R5,11               LENGTH OF LEVELS                             
         EX    R5,LVSPA            ALL SPACES                                   
         BE    DISTB               YES  DISTRIBUTE                              
         SPACE 1                                                                
         LA    R6,LEVATOT                                                       
         L     R5,LEVAL            LEVEL A LENGTH                               
         L     R2,ADACC                                                         
         LA    R2,3(R2)            R2 TO LEVEL A                                
         EX    R5,SAME             TABLE EQUAL RECORD                           
         BNE   RD827               BYPASS                                       
         LA    R3,1(R5,R3)         NEXT LEVEL IN TABLE                          
         LA    R2,1(R5,R2)         NEXT IN RECORD                               
         SPACE 1                                                                
         L     R5,LEVBL            LENGTH OF LEVEL                              
         EX    R5,LVSPA            LEVEL B SPACRS                               
         BE    DISTB               DISTB REGION                                 
         LA    R6,LEVBTOT                                                       
         EX    R5,SAME             LEVEL B EQUAL LEVEL B                        
         BE    DISTB               DISTB MARKET                                 
         B     RD827               BYPASS                                       
         SPACE 1                                                                
SAME     CLC   0(0,R3),0(R2)       R3 EQ TABLE   R2 EQ RECORD                   
         SPACE 2                                                                
DISTB    L     R3,SV3                                                           
         CP    ACDIVAL,0(6,R6)     THIS STORE EQUAL TOTAL                       
         BE    DISTD               YES, STORE GETS FULL AMOUNT                  
         AP    BLDIU,ACDIVAL       ADD STORE UNITS TO TOTAL DISTRIB             
         CP    BLDIU,0(6,R6)       TOTAL UNITS EQUAL                            
         BE    LASTORE             YES, THIS IS LAST STORE                      
         SPACE 1                                                                
         ZAP   WORK(13),BLDIU      TOTAL UNITS SO FAR                           
         MP    WORK(13),BLTOT      X BILL DOLLARS                               
         DP    WORK(13),0(6,R6)    DIV. BY TOTAL UNITS                          
         ZAP   DUB,0(6,R6)         TOTAL UNITS                                  
         OI    DUB+7,X'0F'         FORCE SIGN TO PLUS                           
         OI    WORK+12,X'0F'                                                    
         DP    DUB,=P'2'           TOTAL UNITS BY 2                             
         SPACE 1                                                                
         CP    WORK+7(6),DUB+1(6)  REMAINDER LESS THAN 1/2 TOTAL                
         BL    NOROUND             YES DO NOT ROUND                             
         ZAP   ROUND,WORK+6(1)     SIGN OF DOLLARS                              
         NI    ROUND,X'0F'                                                      
         OI    ROUND,X'10'                                                      
         AP    WORK+1(6),ROUND                                                  
         SPACE 1                                                                
NOROUND  SP    WORK+1(6),BLDIT                                                  
         ZAP   POSTAMT,WORK+1(6)                                                
         AP    BLDIT,WORK+1(6)                                                  
         B     GETPCT                                                           
         SPACE 1                                                                
LASTORE  ZAP   WORK+1(6),BLTOT                                                  
         B     NOROUND                                                          
         SPACE 1                                                                
GETPCT   ZAP   WORK(12),ACDIVAL    STORE UNITS                                  
         MP    WORK(12),=P'10000'                                               
         DP    WORK(12),0(6,R6)    STORE UNITS/TOTAL UNITS                      
         ZAP   POSTPCT,WORK+3(3)   STORE PERCENT                                
         BR    RE                                                               
         SPACE 1                                                                
DISTD    ZAP   POSTAMT,BLTOT                                                    
         ZAP   POSTPCT,=PL3'10000'                                              
         BR    RE                                                               
         SPACE 3                                                                
PRTBDET  NTR1                                                                   
EDPCT    LA    R3,P+89                                                          
         LA    R4,POSTPCT                                                       
         EDIT  (P3,0(R4)),(6,0(R3)),2                                           
PROUT    BAS   RE,DETAIL                                                        
         LA    R6,POSTAMT                                                       
         BAS   R7,PRTOT                                                         
         CLI   QOPT4,C'Y'                     WAS TAPE REQUESTED?               
         BE    LOADDIST                       YES                               
         BAS   RE,REPRT                                                         
         B     XIT                                                              
LOADDIST LA    R1,DISTTAB                     LOAD DISTRIB. TABLE               
COMPMNTH CLI   0(R1),X'00'                    END OF TABLE                      
         BE    INSERT                                                           
         CLC   0(3,R1),INVD                   COMP INVOICE DATE                 
         BNE   BUMPUP                                                           
         AP    6(6,R1),POSTAMT                ADD AMT TO TABLE                  
         BAS   RE,REPRT                       PRINT LINE BUILT ABOVE            
         B     XIT                                                              
BUMPUP   LA    R1,12(R1)                      BUMP TO NEXT ENTRY                
         B     COMPMNTH                                                         
INSERT   MVC   0(3,R1),INVD                   MOVE MONTH TO TABLE               
         MVC   3(3,R1),INVD+5                 MOVE YEAR TO TABLE                
         ZAP   6(6,R1),POSTAMT                MOVE AMT TO TABLE                 
         BAS   RE,REPRT                       PRINT LINE BUILT ABOVE            
         B     XIT                                                              
         EJECT                                                                  
GENWRK   NTR1                                                                   
         CLI   QOPT3,C'N'          DRAFT                                        
         BNE   XIT                 YES IT IS A DRAFT                            
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         LA    R6,AREA                                                          
         MVC   0(2,R6),=H'158'                                                  
         MVC   2(2,R6),=H'0'                                                    
         LA    R6,4(R6)                                                         
         MVI   PSHDEL,X'50'                                                     
         MVI   PSHDLEN,X'46'                                                    
         L     R3,ADACC                                                         
         MVC   PSHDACC,0(R3)                                                    
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC+1(2),BLMDC                                              
         LA    R2,MEDCTB                                                        
         CLC   0(2,R2),0(R7)                                                    
         BE    *+12                                                             
         LA    R2,17(R2)                                                        
         B     *-14                                                             
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(15),2(R2)                                               
         SPACE 2                                                                
         USING TRANSD,R6                                                        
         LA    R6,70(R6)                                                        
         MVI   TRNSEL,X'44'                                                     
         MVI   TRNSLEN,X'1D'                                                    
         MVC   TRNSDATE,BLDAT                                                   
         MVC   TRNSREF,BLINV                                                    
         MVI   TRNSSBRF,X'00'                                                   
         MVI   TRNSTYPE,X'2F'                                                   
         MVI   TRNSSTAT,X'80'                                                   
         MVC   TRNSBTCH,SPACES                                                  
         ZAP   TRNSAMNT,POSTAMT                                                 
         MVC   TRNSANAL,SPACES                                                  
         MVI   TRNSNARR,X'40'                                                   
         SPACE 2                                                                
         USING TRXBILLD,R6                                                      
         LA    R6,29(R6)                                                        
         MVI   TRXBEL,X'4A'                                                     
         MVI   TRXBLEN,X'36'                                                    
         MVC   TRXBMED,2(R2)                                                    
         MVC   TRXBPRD,BLPRD                                                    
         MVC   TRXBEST,BLEST                                                    
         MVC   TRXBMOS,BLMOS                                                    
         MVC   TRXBINV,BLINV                                                    
         MVC   TRXBDAT,BLDAT                                                    
         ZAP   TRXBTOT,BLTOT                                                    
         ZAP   TRXBSHR,POSTPCT                                                  
         LA    R6,54(R6)                                                        
         MVI   0(R6),X'00'                                                      
         SPACE 2                                                                
         USING PSHEADD,R6                                                       
         BAS   RE,ADDPOST                                                       
         AP    TOTCNT,=P'1'                                                     
         AP    TOTCSH,POSTAMT                                                   
         LA    R6,AREA+4                                                        
         MVC   PSHDSBAC,PSHDACC                                                 
         MVC   PSHDSBNM,SPACES                                                  
         L     R3,ADLVCNAM                                                      
         XR    R4,R4                                                            
         IC    R4,1(R3)                                                         
         SH    R4,=H'2'                                                         
         EX    R4,STNAM                                                         
         B     *+10                                                             
STNAM    MVC   PSHDSBNM(0),2(R3)                                                
         MVC   PSHDACC+3(12),SPACES                                             
         L     R3,LEVAD                                                         
         LA    R3,PSHDACC(R3)                                                   
         L     R5,LEVAL                                                         
         EX    R5,MVSTAR                                                        
         L     R3,LEVBD                                                         
         LA    R3,PSHDACC(R3)                                                   
         L     R5,LEVBL                                                         
         EX    R5,MVSTAR                                                        
         L     R3,LEVCD                                                         
         LA    R3,PSHDACC(R3)                                                   
         MVI   0(R3),C'A'                                                       
         LA    R6,70(R6)                                                        
         USING TRANSD,R6                                                        
         MVI   TRNSSTAT,X'00'                                                   
         SPACE 2                                                                
         BAS   RE,ADDPOST                                                       
         AP    TOTCNT,=P'1'                                                     
         B     XIT                                                              
MVSTAR   MVC   0(0,R3),=12C'*'                                                  
         EJECT                                                                  
         EJECT                                                                  
ADDSUM   NTR1                                                                   
         LA    R5,MEDIAS                                                        
         LA    R6,7                                                             
GETCD    CLC   BLMDC,0(R5)                                                      
         BE    GTDIS                                                            
         LA    R5,3(R5)                                                         
         BCT   R6,GETCD                                                         
         LA    R6,36                                                            
         B     ADTB                                                             
GTDIS    XR    R6,R6                                                            
         IC    R6,2(R5)                                                         
ADTB     LA    R5,SUMBUK                                                        
         LA    R5,0(R6,R5)                                                      
         AP    0(6,R5),POSTAMT                                                  
         AP    SUMBUKT,POSTAMT                                                  
         B     XIT                                                              
         EJECT                                                                  
*              PROCTRNS                                                         
         SPACE 2                                                                
RD900    L     R6,ADTRANS                                                       
         CLI   TRNSEL,X'44'                                                     
         BNE   RDEXT                                                            
         TM    TRNSSTAT,X'80'      ONLY DEBITS                                  
         BNO   RDEXT                                                            
         SPACE 1                                                                
         CLI   START,0             SPECIAL YEARLY HAS START AND END             
         BE    RD900A              NOT A SPECIAL                                
         TM    TRNSSTAT,X'20'                                                   
         BO    RDEXT               IGNORE REVERSALS                             
         CLC   TRNSDATE,START                                                   
         BL    RDEXT                                                            
         CLC   TRNSDATE,END                                                     
         BH    RDEXT                                                            
         B     RD900B                                                           
         SPACE 1                                                                
RD900A   LR    RF,R6                                                            
         SH    RF,DATADISP                                                      
         USING ACKEYD,RF                                                        
         OC    ACDTUSED,ACDTUSED                                                
         BZ    *+14                NOT USED - OK                                
         CLC   ACDTUSED,TODAY2     USED TODAY MUST BE RERUN                     
         BNE   RDEXT                                                            
         TM    TRNSSTAT,X'20'       REVERSAL                                    
         BO    RD913                                                            
         SPACE 2                                                                
         USING TRSUBHD,R3                                                       
RD900B   LA    R8,MEDCTB           MEDIA CODE TABLE                             
         LA    R5,15                                                            
         L     R3,ADSUBAC                                                       
RD901    CLC   0(2,R8),TRSBACNT+1  MEDIA CODE IN TABLE                          
         BE    RD903                                                            
         CLI   0(R8),0             MORE ENTRIES                                 
         BNE   RD905                  YES                                       
RD903    MVC   0(2,R8),TRSBACNT+1                                               
         MVI   ELCODE,X'4A'                                                     
         LR    R3,R6                                                            
         BAS   RE,NEXTEL                                                        
         BNE   *+2                                                              
         LR    R6,R3                                                            
         USING TRXBILLD,R6                                                      
         MVC   2(15,R8),TRXBMED    MEDIA NAME TO TABLE                          
         B     RD910                                                            
         SPACE 2                                                                
RD905    LA    R8,17(R8)                                                        
         BCT   R5,RD901                                                         
         B     *+2                                                              
         SPACE 2                                                                
RD910    L     R7,=V(BILLTAB)                                                   
         A     R7,RELO                                                          
         L     R2,=AL4(BLEND-BLSTR)                                             
         MH    R2,CNT+2                                                         
         AR    R7,R2                                                            
         MVC   BLMDC,0(R8)       MEDIA CODE TO BILL TABLE                       
         MVI   BLMDC+2,X'40'                                                    
         MVC   BLPRD,TRXBPRD       PRODUCT CODE                                 
         MVC   BLEST,TRXBEST       ESTIMATE                                     
         MVC   BLMOS,TRXBMOS       MONTH OF SERVICE                             
         MVC   BLINV,TRXBINV       INVOICE NUMBER                               
         MVC   BLDAT,TRXBDAT       INVOICE DATE                                 
         ZAP   BLTOT,TRXBTOT       BILL TOTAL                                   
         L     R3,ADSUBAC                                                       
         MVC   BLACC,TRSBACNT+3    REGION/MARKET                                
         ZAP   BLDIU,=P'0'                                                      
         ZAP   BLDIT,=P'0'                                                      
         L     R8,CNT                                                           
         LA    R8,1(R8)                                                         
         ST    R8,CNT                                                           
         SPACE 2                                                                
         L     R6,ADTRANS                                                       
         USING TRANSD,R6                                                        
         CLC   BLDAT,SPACES                                                     
         BNE   *+10                                                             
         MVC   BLDAT,TRNSDATE                                                   
         CLI   QOPT3,C'N'      DRAFT                                            
         BNE   RDEXT      YES,  DO NOT WRITE TO FILE                            
RD913    LR    RF,R6                                                            
         SH    RF,DATADISP                                                      
         USING ACKEYD,RF                                                        
         MVC   ACDTUSED,TODAY2                                                  
         MVI   MODE,WRITRANS                                                    
         B     RDEXT                                                            
         EJECT                                                                  
*              REQLAST                                                          
         SPACE 2                                                                
RDA00    BAS   RE,SPAC2            SKIP LINE                                    
         MVC   P+76(16),=C'TOTAL ADVERTISER'                                    
         LA    R6,ADVDIS                                                        
         LA    R8,P+94                                                          
         EDIT  (P6,0(R6)),(14,0(R8)),2,COMMAS=YES,MINUS=YES                     
         BAS   RE,REPRT                                                         
         MVC   PAGE,=H'1'                                                       
         SPACE 1                                                                
         LA    R5,REGTOT                                                        
         LA    R6,ALLTOT                                                        
         LA    R7,8                                                             
LASTOT   ZAP   0(6,R5),=P'0'                                                    
         ZAP   0(6,R6),=P'0'                                                    
         LA    R5,6(R5)                                                         
         LA    R6,6(R6)                                                         
         BCT   R7,LASTOT                                                        
         SPACE 1                                                                
         MVC   SUMTB(2),=C'/*'                                                  
         PUT   RETLWRK,SUMTB                                                    
         CLOSE (RETLWRK)                                                        
         SPACE 1                                                                
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         OPEN  (RETLWRK,(INPUT))                                                
PRTSUM   GET   RETLWRK,SUMTB                                                    
         CLC   SUMTB(2),=C'/*'                                                  
         BE    EOF                                                              
         SPACE 1                                                                
         CLI   SUMTB+1,X'FF'                                                    
         BNE   NOTRG                                                            
         LA    R6,REGTOT                                                        
         LA    R7,ALLTOT                                                        
         MVC   P+6(16),=C'TOTAL FOR REGION'                                     
         BAS   RE,SUMLINE                                                       
         B     PRTSUM                                                           
         SPACE 1                                                                
NOTRG    CLI   SUMTB+1,X'40'                                                    
         BNE   MRKTOT                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVRGN,SUMKY                                                      
         MVC   SVRGNM,SUMNM                                                     
         B     PRTSUM                                                           
         SPACE 1                                                                
MRKTOT   LA    R5,SUMKY                                                         
         A     R5,LEVAL                                                         
         LA    R5,1(R5)                                                         
         L     R3,LEVBL                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         SPACE 1                                                                
         LA    R5,SUMNM                                                         
         LA    R6,36                                                            
MKLOP    CLI   0(R5),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         LA    R5,1(R5)                                                         
         BCT   R6,MKLOP                                                         
         SPACE 1                                                                
         GOTO1 CHOPPER,DMCB,(36,SUMNM),(24,P+5),(C'P',2)                        
         LA    R6,SUMBUK                                                        
         LA    R7,REGTOT                                                        
         BAS   RE,SUMLINE                                                       
         B     PRTSUM                                                           
         SPACE 1                                                                
EOF      MVC   P+6(20),=C'TOTAL FOR ADVERTISER'                                 
         LA    R6,ALLTOT                                                        
         LA    R7,SUMBUK                                                        
         BAS   RE,SUMLINE                                                       
         CLOSE (RETLWRK)                                                        
         CLI   QOPT4,C'Y'                                                       
         BNE   RDEXT                                                            
         CLOSE RETTAPE                                                          
         B     RDEXT                                                            
         EJECT                                                                  
*              RUNLAST                                                          
         SPACE 2                                                                
         USING PSSUBFD,R6                                                       
RDB00    LA    R6,AREA                                                          
         CP    TOTCNT,=P'0'        ANY WORKER RECORDS                           
         BE    RDEXT                                                            
         MVC   0(2,R6),=H'34'                                                   
         MVC   2(2,R6),=H'0'                                                    
         LA    R6,4(R6)                                                         
         MVI   PSSBEL,X'52'                                                     
         MVI   PSSBLEN,X'1D'                                                    
         MVC   PSSBDESC,=C'RETAIL DISTRIBU'                                     
         ZAP   PSSBRECS,TOTCNT                                                  
         ZAP   PSSBCASH,TOTCSH                                                  
         LA    R6,29(R6)                                                        
         MVI   0(R6),0                                                          
         BAS   RE,ADDPOST                                                       
         BAS   RE,CLOSPOST                                                      
         B     RDEXT                                                            
         EJECT                                                                  
*                                                                               
PRTOT    LA    R8,P+97                                                          
         EDIT  (P6,0(R6)),(11,0(R8)),2,COMMAS=YES,MINUS=YES                     
         BR    R7                                                               
         SPACE 3                                                                
         USING ACNAMED,R3                                                       
HEADUP   NTR1                      GET ADVERTISER NAME                          
         L     R3,ADLDGNAM                                                      
         MVC   HEAD4+1(10),=C'ADVERTISER'                                       
         SR    R4,R4                                                            
         IC    R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         LA    R5,HEAD4+12                                                      
         EX    R4,PNAM                                                          
         MVC   HEAD5+84(6),=C'SCHEME'                                           
         MVC   HEAD5+91(2),SCHCD                                                
         CLI   QOPT3,C'N'          DRAFT                                        
         BE    *+10                                                             
         MVC   HEAD5+95(5),=C'DRAFT'                                            
         CLI   RCSUBPRG,0                                                       
         BE    XIT                                                              
         SPACE 3                                                                
         MVC   HEAD5+1(6),=C'REGION'                                            
         CLI   RCSUBPRG,2                                                       
         BE    HEADSUM                                                          
         L     R3,ADACC                                                         
         LA    R3,3(R3)                                                         
         L     R6,LEVAL                                                         
         EX    R6,CODOUA           LEVEL A CODE (REGION) TO HEAD                
         L     R3,ADLVANAM                                                      
         SR    R4,R4                                                            
         IC    R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         LA    R5,HEAD5+8                                                       
         LA    R5,2(R6,R5)                                                      
         EX    R4,PNAM                                                          
         SPACE 3                                                                
         MVC   HEAD6+1(6),=C'MARKET'                                            
         L     R3,ADACC                                                         
         A     R3,LEVBD                                                         
         L     R6,LEVBL                                                         
         EX    R6,CODOUB           LEVEL B CODE (MARKET) TO HEAD                
         L     R3,ADLVBNAM                                                      
         SR    R4,R4                                                            
         IC    R4,ACNMLEN                                                       
         SH    R4,=H'3'                                                         
         LA    R5,HEAD6+8                                                       
         LA    R5,2(R6,R5)                                                      
         EX    R4,PNAM                                                          
         B     XIT                                                              
         SPACE 1                                                                
HEADSUM  L     R5,LEVAL                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+8(0),SVRGN                                                 
         MVC   HEAD5+10(36),SVRGNM                                              
         B     XIT                                                              
*                                                                               
PNAM     MVC   0(0,R5),ACNMNAME                                                 
CODOUA   MVC   HEAD5+8(0),0(R3)                                                 
CODOUB   MVC   HEAD6+8(0),0(R3)                                                 
         SPACE 2                                                                
SPAC2    NTR1                                                                   
         BAS   RE,HEADUP                                                        
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 1                                                                
REPRT    NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
STORTOT  NTR1                                                                   
         CP    LEVCDIS,POSTAMT                                                  
         BE    STOR2                                                            
         BAS   RE,REPRT                                                         
         MVC   P+76(15),=C'TOTAL FOR STORE'                                     
         LA    R6,LEVCDIS                                                       
         BAS   R7,PRTOT                                                         
         MVI   SPACING,2                                                        
         BAS   RE,REPRT            PRINT                                        
STOR2    BAS   RE,REPRT                                                         
         CLI   QOPT4,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,TOTTAPE                                                       
         AP    LEVBDIS,LEVCDIS                                                  
         SP    LEVCDIS,LEVCDIS                                                  
         B     XIT                                                              
         EJECT                                                                  
TOTTAPE  NTR1                                                                   
         LA    R4,DISTTAB                     DISTRIBUTION AMT TABLE            
         LA    R5,TREC                        O/P RECORD                        
         USING TAPEDSEC,R5                                                      
         LA    R3,SVACC                       ACCOUNT NUMBER                    
         A     R3,LEVCD                       DISPLACEMENT TO STORE #           
         PACK  WORKFLD(3),0(4,R3)                                               
         UNPK  TASTORE(5),WORKFLD(3)          MAKE 5 IN LENGTH                  
         OI    TASTORE+4,X'F0'                                                  
         MVI   TACON4,C'4'                    CONSTANT                          
         MVC   TACON056,=C'0506'              CONSTANT                          
         MVC   TABLANKS,=54C' '               CONSTANT                          
DBTAMT   MVC   TAACOUNT(6),=C'331251'         DEBIT ACCT NO                     
         UNPK  TAAMOUNT(9),6(6,R4)            DIST AMOUNT                       
         LA    R2,MNTHTABL                    PICK UP MONTH FROM                
CKTAB    CLI   0(R2),X'FF'                    MONTH TABLE                       
         BE    XIT                            ERROR CONDITION                   
         CLC   0(3,R2),0(R4)                                                    
         BE    PUTIT                          MONTHS EQUAL                      
         LA    R2,4(R2)                       LOOK AT NEXT ENTRY                
         B     CKTAB                                                            
PUTIT    MVC   TAMONTH(1),3(R2)               MOVE IN MONTH                     
         PUT   RETTAPE,TREC                                                     
CRTAMT   MVC   TAACOUNT(6),=C'191401'         CREDIT ACCOUNT NO                 
         ZAP   PACKWORK(6),6(6,R4)                                              
         MP    PACKWORK(6),=P'-1'                MAKE NEGATIVE                  
         UNPK  TAAMOUNT(9),PACKWORK(6)                                          
         PUT   RETTAPE,TREC                                                     
BUMPNEXT LA    R4,12(R4)                      LOOK AT NEXT DISTBL ENTRY         
         OC    0(12,R4),0(R4)                                                   
         BNE   DBTAMT                         MORE ENTRIES                      
         XC    DISTTAB,DISTTAB                CLEAR TABLE FOR NEXT PASS         
         B     XIT                                                              
         SPACE 2                                                                
MEDTOT   NTR1                                                                   
         BAS   RE,REPRT                                                         
         MVC   P+60(11),=C'MEDIA TOTAL'                                         
         LA    R2,P+77                                                          
         EDIT  LEVCDIS,(11,0(R2)),2,MINUS=YES                                   
         BAS   RE,REPRT                                                         
         SP    LEVCDIS,LEVCDIS                                                  
         B     XIT                                                              
         EJECT                                                                  
*              SET-UP DETAIL LINE                                               
DETAIL   NTR1                                                                   
         MVC   MDCD,BLMDC                                                       
         MVC   PRCD,BLPRD                                                       
         MVC   ESTN,BLEST                                                       
         MVC   MONSV,BLMOS                                                      
         MVC   INVN,BLINV                                                       
         GOTO1 DATCON,DMCB,(1,BLDAT),(8,INVD)                                   
         LA    R2,INVA                                                          
         EDIT  BLTOT,(10,0(R2)),2,MINUS=YES                                     
         B     XIT                                                              
         EJECT                                                                  
SUMLINE  NTR1                                                                   
         CLC   0(6,R6),SPACES      NO ACTIVITY                                  
         BE    XIT                                                              
         CLC   0(6,R7),SPACES      NO ACTIVITY                                  
         BE    XIT                                                              
         LA    R3,8                                                             
         LA    R5,P+30                                                          
SUMLP    AP    0(6,R7),0(6,R6)                                                  
         CP    0(6,R6),=P'0'                                                    
         BL    SPT                                                              
         AP    0(6,R6),=P'50'                                                   
         B     ZAPT                                                             
SPT      SP    0(6,R6),=P'50'                                                   
ZAPT     ZAP   WORK(8),0(6,R6)                                                  
         DP    WORK(8),=P'100'                                                  
         EDIT  (P6,WORK),(9,0(R5)),MINUS=YES                                    
         ZAP   0(6,R6),=P'0'                                                    
         LA    R6,6(R6)                                                         
         LA    R7,6(R7)                                                         
         LA    R5,10(R5)                                                        
         BCT   R3,SUMLP                                                         
         SPACE 1                                                                
         BAS   RE,REPRT                                                         
         BAS   RE,REPRT                                                         
         B     XIT                                                              
         EJECT                                                                  
         USING ACDISTD,R3                                                       
GET62    MVI   ELCODE,X'62'        GET 62 ELEMENT WITH                          
*                                  MATCHING SCHEME CODE                         
         BAS   RE,GETEL                                                         
         BNE   0(R6)                                                            
         CLC   ACDICODE,SCHCD                                                   
         BE    4(R6)                                                            
         BAS   RE,NEXTEL                                                        
         BE    *-14                                                             
         BR    R6                                                               
*                                                                               
         SPACE 3                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
ADDPOST  MVC   COMMAND,=CL6'ADD'                                                
         B     FILE                                                             
         SPACE 1                                                                
CLOSPOST MVC   COMMAND,=CL6'CLOSE'                                              
         B     FILE                                                             
         SPACE 1                                                                
FILE     NTR1                                                                   
*                                                                               
         LA    R6,AREA                                                          
         L     R4,=A(POSTBUFF)                                                  
         A     R4,RELO                                                          
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R6)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
RETLWRK  DCB   DDNAME=RETLWRK,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00099,                                            X        
               BLKSIZE=00099,          DOS BLKSIZE=00099               X        
               MACRF=(PM,GM)           CHECK                                    
         SPACE 3                                                                
DDPARM   DC    CL8'RETTAPE '                                                    
DSPARM   DC    CL20'ACCTAPE.AC047SW1'                                           
         SPACE 3                                                                
RETTAPE  DCB   DDNAME=RETTAPE,                                         X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=02400,                                          X        
               MACRF=PM                                                         
         SPACE 3                                                                
SUMTB    DS    0CL99                                                            
SUMKY    DS    CL15                                                             
SUMNM    DS    CL36                                                             
SUMBUK   DS    7PL6                                                             
SUMBUKT  DS    PL6                                                              
         SPACE 2                                                                
MEDIAS   DC    C'SN',X'00'                                                      
         DC    C'SX',X'06'                                                      
         DC    C'ST',X'0C'                                                      
         DC    C'SR',X'12'                                                      
         DC    C'PM',X'18'                                                      
         DC    C'PS',X'18'                                                      
         DC    C'PN',X'1E'                                                      
         SPACE 2                                                                
MNTHTABL DC    C'JAN1'                                                          
         DC    C'FEB2'                                                          
         DC    C'MAR3'                                                          
         DC    C'APR4'                                                          
         DC    C'MAY5'                                                          
         DC    C'JUN6'                                                          
         DC    C'JUL7'                                                          
         DC    C'AUG8'                                                          
         DC    C'SEP9'                                                          
         DC    C'OCT0'                                                          
         DC    C'NOV-'                                                          
         DC    C'DEC*'                                                          
         DC    X'FF'                                                            
REGTOT   DS    8PL6                                                             
ALLTOT   DS    8PL6                                                             
         SPACE 1                                                                
DUMEOF   DC    F'0'                                                             
FIRSTIME DC    C'Y'                                                             
SVRGN    DS    CL15                                                             
SVRGNM   DS    CL36                                                             
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR BILL TABLE                                             
BLTBD    DSECT                                                                  
BLSTR    EQU   *                                                                
BLMDC    DS    CL2       MEDIA CODE                                             
         DS    CL1                                                              
BLACC    DS    CL12      REGION/MARKET                                          
BLPRD    DS    CL6       PRODUCT CODE                                           
BLEST    DS    CL3       ESTIMATE NUMBER                                        
BLMOS    DS    CL6       MONTH OF SERVICE (MMM/YY)                              
BLINV    DS    CL10      INV. NUMBER                                            
BLDAT    DS    PL3       INVOICE DATE (YMD)                                     
BLTOT    DS    PL6       BILL TOTAL                                             
BLDIU    DS    PL6                 UNITS DISTRIBUTED                            
BLDIT    DS    PL6                 DOLLARS DISTRIBUTED                          
BLEND    EQU   *                                                                
         EJECT                                                                  
AC4702D  DSECT                DSECT FOR RE47 WORKAREA                           
RELO     DS    F                                                                
ID       DS    CL16                                                             
TOTCNT   DS    PL6            NUMBER RECORDS ADDED                              
TOTCSH   DS    PL6            TOTAL VALUE ADDED                                 
*                                                                               
POSTAMT  DS    PL6                                                              
POSTPCT  DS    PL3                                                              
*                                                                               
MEDCTB   DS    15CL17         2 BYTE CODE, 15 BYTE NAME                         
*                                                                               
ELCODE   DS    CL1                                                              
*                                                                               
SCHCD    DS    CL2                 SCHEME CODE                                  
*                                                                               
CNT      DS    F                                                                
*                                                                               
ADVTOT   DS    PL6                 TOTAL ADVERTISER UNITS                       
LEVATOT  DS    PL6                       MARKET UNITS                           
LEVBTOT  DS    PL6                       REGION UNITS                           
*                                                                               
ADVDIS   DS    PL6                 ADVERTISER DISTRIBUTED TOTAL                 
LEVADIS  DS    PL6                 LEVEL DISTRIBUTED TOTAL                      
LEVBDIS  DS    PL6                 LEVEL B DISTRIBUTED TOTAL                    
LEVCDIS  DS    PL6                 LEVEL C DISTRIBUTED TOTAL                    
TODAY2   DS    CL2                                                              
*                                                                               
LEVAD    DS    F                   LEVEL A DISP                                 
LEVAL    DS    F                   LEVEL A LENGTH                               
LEVBD    DS    F                   LEVEL B DISP                                 
LEVBL    DS    F                   LEVEL B LENGTH                               
LEVCD    DS    F                   LEVEL C DISP                                 
LEVCL    DS    F                   LEVEL C LENGTH                               
*                                                                               
ROUND    DS    PL1                                                              
START    DS    CL3                                                              
END      DS    CL3                                                              
*                                                                               
SVACC    DS    CL15                                                             
SV3      DS    F                                                                
*                                                                               
COMMAND  DS    CL6                                                              
AREA     DS    0F                                                               
         DS    CL200                                                            
WORKFLD  DS    PL3                                                              
PACKWORK DS    PL6                                                              
TREC     DS    CL80                                                             
DISTTAB  DS    CL144                                                            
         SPACE 3                                                                
         EJECT                                                                  
*                DSECT FOR OPTIONAL OUTPUT TAPE                                 
TAPEDSEC DSECT                                                                  
TAPEREC  DS    0CL80                                                            
TACON4   DS    CL1                                                              
TASTORE  DS    CL5                                                              
TAMONTH  DS    CL1                                                              
TAACOUNT DS    CL6                                                              
TAAMOUNT DS    CL9                                                              
TACON056 DS    CL4                                                              
TABLANKS DS    CL54                                                             
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE ACGENBOTH                                                      
         EJECT                                                                  
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*              DSECT FOR DETAIL PRINT LINE                                      
         ORG   P+31                                                             
MDCD     DS    CL2                 MEDIA CODE                                   
         DS    CL3                                                              
PRCD     DS    CL6                 PRODUCT CODE                                 
         DS    CL2                                                              
ESTN     DS    CL3                 ESTIMATE                                     
         DS    CL4                                                              
MONSV    DS    CL6                 MONTH OF SERVICE                             
         DS    CL1                                                              
INVN     DS    CL10                INVOICE NUMBER                               
         DS    CL1                                                              
INVD     DS    CL8                 INVOICE DATE                                 
         DS    CL1                                                              
INVA     DS    CL10                INVOICE AMOUNT                               
         DS    CL1                                                              
STPC     DS    CL6                 STORE PERCENT                                
         DS    CL2                                                              
STAMT    DS    CL11                                                             
         ORG                                                                    
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACGENMODES                                                     
BILLTAB  CSECT                                                                  
         DS    1000CL60                                                         
POSTBUFF CSECT                                                                  
         DC    4500X'00'                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACREP4702 05/01/02'                                      
         END                                                                    
