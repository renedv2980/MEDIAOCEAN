*          DATA SET SPREPFXSPX AT LEVEL 001 AS OF 10/04/00                      
*          DATA SET SPREPFXSPL AT LEVEL 137 AS OF 01/12/00                      
*PHASE SPFX02K                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPFX02 - DEL DUPLICATE 03 ELEMS FROM SPOT BUY RECS'             
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
* SORT THE INPUT FILE                                                           
*                                                                               
FX       OPEN  (RECVIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(12),DMCB                                                    
         L     RE,=A(SORTCARD)                                                  
         ST    RE,DMCB+0                                                        
         L     RE,=A(RECCARD)                                                   
         ST    RE,DMCB+4                                                        
         GOTO1 =V(SORTER),DMCB                                                  
*                                                                               
         L     R6,ADBUY                                                         
         SHI   R6,4                                                             
*                                                                               
FX2      GET   RECVIN,(R6)                                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R6)                                     
         B     FX2                                                              
*                                                                               
FX8      CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         XC    AGY,AGY             INITIALIZE                                   
*                                                                               
FX10     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RE,15,4(R1)                                                      
         BZ    FX200                                                            
* MOVE SORT RECORD SO CAN SEE IT                                                
         L     R6,ADBUY                                                         
         SR    R7,R7                                                            
         ICM   R7,3,0(RE)                                                       
         LA    RE,28(RE)           POINT TO BUY RECORD                          
         SHI   R7,28                                                            
         LR    RF,R7                                                            
         MVCL  (R6),(RE)                                                        
* READ FOR BUY IN  CASE IT'S DELETED                                            
         L     R6,ADBUY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(10),0(R6)                                                    
         MVC   KEY+11(1),10(R6)                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   FX10                                                             
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         SR    R7,R7                                                            
         ICM   R7,3,13(R6)                                                      
         AR    R7,R6                                                            
         XC    0(2,R7),0(R7)       SET EOR FLAG                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,(R6),P+40,13                                         
*                                                                               
         CLC   AGY,BUYALPHA        TEST CHANGE OF AGY                           
         BE    FX12                                                             
         MVC   AGY,BUYALPHA        SET IN SPREPWORK                             
         BAS   RE,READSPL          GET NEW SPILL TABLE                          
*                                                                               
FX12     LA    R4,P                                                             
         MVC   0(2,R4),BUYALPHA                                                 
*                                                                               
         L     RE,ADCLT                                                         
         CLC   1(3,RE),0(R6)       HAVE CLIENT RECORD                           
         BE    FX20                                                             
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),0(R6)      MOVE A-M/CLT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETCLT                                                           
*                                                                               
FX20     GOTO1 CLUNPK,DMCB,1(R6),4(R4)                                          
*                                                                               
         LA    R1,WORK                                                          
         XC    0(32,R1),0(R1)                                                   
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,BUYALPHA                                                 
         MVI   STAPMED,C'N'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPMKT(5),BUYMSTA                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   10(5,R4),STAPQSTA                                                
         MVC   STA,STAPQSTA        SAVE IN SPREPWORK                            
         DROP  R1                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,BUYKEST                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  17(3,R4),DUB                                                     
*                                                                               
         MVI   20(R4),C'-'                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,BUYKBUY                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  21(3,R4),DUB                                                     
*                                                                               
         MVC   26(4,R4),BDPROGRM   PRINT SHOW CODE                              
* CHECK FOR A DEMO OVERRIDE RECORD                                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D17'                                                  
         MVC   KEY+2(1),BUYKAM                                                  
* FIND A X'68' ELEMENT TO GET NTWK SEQNUM                                       
         BAS   RE,GETNET                                                        
         MVC   KEY+3(1),BYTE       NETOWRK SEQNUM                               
         MVC   KEY+4(2),BUYKCLT                                                 
         MVC   KEY+7(4),BDPROGRM                                                
         L     RE,ADCLT                                                         
         USING CLTHDRD,RE                                                       
         MVC   KEY+11(1),CPROF+3      RTGSVC                                    
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    FX22                                                             
* NO CLIENT EXCEPTION FOUND, TRY DEFAULT                                        
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+4(2),KEY+4      CLEAR CLIENT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   *+8                                                              
FX22     MVI   30(R4),C'*'                                                      
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         BAS   RE,GETXP            PROCESS EXPLODED BUYS                        
         B     FX10                                                             
*                                                                               
GETNET   NTR1                                                                   
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCODE,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   BYTE,6(R6)                                                       
         B     EXIT                                                             
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=4000,MACRF=GM,    X        
               EODAD=FX8                                                        
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(29,13,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2000'                                  
*                                                                               
FX100    DS    0H                                                               
FX200    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
GETXP    NTR1                                                                   
         LA    R6,BDELEM                                                        
*                                                                               
GETXP2   MVI   ELCODE,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
* BUILD ARGUMENT FOR BINSRCH                                                    
         XC    DUB,DUB                                                          
         L     RE,ADCLT                                                         
         USING CLTHDRD,RE                                                       
         MVC   DUB(1),CPROF+3      RTGSVC                                       
         DROP  RE                                                               
*                                                                               
         LA    R1,WORK                                                          
         XC    0(32,R1),0(R1)                                                   
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGY                                                      
         MVI   STAPMED,C'N'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPMKT(5),2(R6)   X'68' MKT/STA                                 
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DUB+1(5),STAPQSTA   MOVE STATION                                 
         MVI   DUB+5,0             LAST BYTE OF STA X'00'                       
         L     RE,ADBUY                                                         
         MVC   DUB+6(2),1(RE)      MOVE CLIENT                                  
         DROP  R1                                                               
*                                                                               
         MVI   CLTSPILL,C'N'                                                    
         L     R0,SPILLCNT                                                      
         GOTO1 BINSRCH,DMCB,DUB,A(SPILLTAB),(R0),L'SPILLTAB,(0,8)               
         CLI   0(R1),0             TEST FOUND                                   
         BNE   GETXP4                                                           
         MVI   CLTSPILL,C'Y'                                                    
         B     GETXP10                                                          
*                                                                               
GETXP4   XC    DUB+6(2),DUB+6      CLEAR CLIENT                                 
*                                                                               
         MVI   0(R1),0             RESET                                        
         GOTO1 (RF),(R1)                                                        
         CLI   0(R1),0                                                          
         BNE   GETXP2                                                           
* THERE IS SPILL - GET THE BUY RECORD NOW                                       
GETXP10  MVC   SVBINADR,0(R1)      SAVE TABLE ENTRY ADDRESS                     
*                                                                               
         XC    KEY,KEY                                                          
         L     RE,ADBUY                                                         
         MVC   KEY(10),0(RE)       MOVE NETWORK BUY KEY                         
         MVC   KEY+11(1),10(RE)    LINE NUMBER                                  
         MVC   KEY+4(5),2(R6)      MOVE X'68' MKT/STA                           
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    GETXP12                                                          
         MVC   P(20),=C'MISSING EXPLODED BUY'                                   
         L     RE,SVBINADR                                                      
         MVC   P+22(4),1(RE)                                                    
         GOTO1 REPORT                                                           
         B     GETXP2                                                           
*                                                                               
GETXP12  L     R0,=A(MYBUY)                                                     
         ST    R0,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         BAS   RE,FIXBUY                                                        
         BNE   GETXP2                                                           
*                                                                               
         BAS   RE,BLDPRDLS         BUILD PRODUCT LIST                           
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   GETXP14                                                          
*                                                                               
         GOTO1 PUT                                                              
GETXP14  XC    DMCB+20(4),DMCB+20                                               
*                                                                               
         BAS   RE,PUTPTRS          ADD PASSIVE POINTERS                         
*                                                                               
GETXP16  CLI   CLTSPILL,C'Y'                                                    
         BNE   *+10                                                             
         MVC   P(6),=C'CLIENT'                                                  
         MVC   P+8(16),=C'SPILL ADDED FOR '                                     
         L     RE,SVBINADR                                                      
         MVC   P+20(4),1(RE)                                                    
         GOTO1 REPORT                                                           
         MVI   CLTSPILL,C'N'                                                    
         B     GETXP2                                                           
CLTSPILL DC    C'N'                                                             
         EJECT                                                                  
FIXBUY   NTR1                                                                   
         L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,NEXTEL                                                        
         BE    NEQXIT              IF SPILL IN RECORD, IT'S OK                  
* BUILD SPILL DEMO ELEMENT                                                      
         L     R6,AREC                                                          
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM(24),0(R6)                                                   
         MVI   ELEM,3              SET SPILL DEMEL CODE                         
         MVI   ELEM+1,24           RESET LENGTH                                 
* MOVE ALL RATINGS TO SPILL ELEM                                                
         SR    R0,R0                                                            
         ZIC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   NEQXIT                                                           
         SRL   R0,3                SET FOR BCT                                  
         LA    R1,ELEM+24                                                       
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
FIXB10   CLI   1(R6),C'R'                                                       
         BE    FIXB12                                                           
         CLI   1(R6),C'E'                                                       
         BNE   FIXB14                                                           
FIXB12   XC    0(8,R1),0(R1)                                                    
         MVC   0(3,R1),0(R6)       MOVE DEMO DESC                               
         LA    R1,8(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,ELEM+1           BUMP ELEMENT LENGTH                          
         LA    RE,8(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
FIXB14   LA    R6,8(R6)                                                         
         BCT   R0,FIXB10                                                        
* ADD AN 03 ELEMENT FOR EACH SPILL MARKET                                       
         L     R6,AREC             FIND DEMO ELEMENT AGAIN                      
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,SVBINADR         POINT TO SAVED TABLE ENTRY                   
         LA    R4,8(R4)            POINT TO FIRST MARKET                        
         LA    R5,10                                                            
*                                                                               
FIXB20   MVC   ELEM+4(4),0(R4)     MOVE MARKET NUMBERS                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               POINT BEYOND LAST DEMO ELEMENT               
                                                                                
FIXB22   GOTO1 RECUP,DMCB,AREC,ELEM,(R6)                                        
         LA    R4,4(R4)                                                         
         OC    0(2,R4),0(R4)       TEST MORE MARKETS                            
         BZ    FIXBX                                                            
         BCT   R5,FIXB20                                                        
*                                                                               
FIXBX    BAS   RE,PRTFIX                                                        
         B     EQXIT                                                            
         EJECT                                                                  
PRTFIX   NTR1                                                                   
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
*                                                                               
         L     R6,AREC                                                          
         LA    R6,BDELEM                                                        
         MVI   ELCODE,3                                                         
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,NEXTEL           FIND ANOTHER SPILL ELEM                      
         BNE   EXIT                                                             
*                                                                               
         CP    FIXCOUNT,=P'10'                                                  
         BH    EXIT                                                             
         AP    FIXCOUNT,=P'1'                                                   
*                                                                               
         L     RE,AREC                                                          
         SR    R0,R0                                                            
         ICM   R0,3,13(RE)                                                      
         GOTO1 PRNTBL,DMCB,=C'FIXED BUY',AREC,C'DUMP',(R0),=C'1D00'             
         B     EXIT                                                             
FIXCOUNT DC    PL4'0'                                                           
*                                                                               
         EJECT                                                                  
READSPL  NTR1                                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   FIXCOUNT,=P'0'      RESET                                        
         MVC   P(28),=C'BUILDING SPILL TABLE NOW FOR'                           
         MVC   P+29(2),AGY                                                      
         GOTO1 REPORT                                                           
*                                                                               
         L     RE,=A(SPILLTAB)     CLEAR SPILL TABLE                            
         L     RF,=A(SPILLTBX-SPILLTAB)                                         
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D13'                                                  
         MVC   KEY+2(2),AGY                                                     
*                                                                               
         L     R4,=A(SPILLTAB)                                                  
         GOTO1 HIGH                                                             
         B     RDSP4                                                            
*                                                                               
RDSP2    GOTO1 SEQ                                                              
*                                                                               
RDSP4    CLC   KEY(4),KEYSAVE      SAME TYPE/AGY                                
         BNE   READSPLX                                                         
*                                                                               
         L     R6,=A(MYBUY)                                                     
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         MVC   0(8,R4),4(R6)       MOVE RTGSVC/STATION(5)/CLT(2)                
*                                                                               
         LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
*                                                                               
         LA    RE,8(R4)            POINT TO TABLE MKT LIST                      
         LA    RF,(L'SPILLTAB-8)/4 MAX SPILL MKTS                               
*                                                                               
RDSP10   CLI   0(R6),0                                                          
         BE    RDSP14                                                           
         CLI   0(R6),5                                                          
         BNE   RDSP12                                                           
         TM    8(R6),X'80'         TEST TO IGNORE                               
         BO    RDSP12                                                           
* MOVE MARKET TO TABLE                                                          
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                DECREMENT                                    
         MVC   0(4,RE),2(R6)       MOVE AGY MKT/RTGSVC MKT                      
         LA    RE,4(RE)                                                         
*                                                                               
RDSP12   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RDSP10                                                           
*                                                                               
RDSP14   LA    R0,(L'SPILLTAB-8)/4 MAX SPILL MKTS                               
         BCTR  R0,0                                                             
         CR    RF,R0                                                            
         BNL   RDSP16                                                           
*&&DO                                                                           
         MVC   P(11),=C'SPILL ENTRY'                                            
         MVC   P+12(5),0(R4)                                                    
         GOTO1 HEXOUT,DMCB,6(R4),P+18,2,=C'TOG'                                 
         GOTO1 REPORT                                                           
*&&                                                                             
RDSP16   OC    8(4,R4),8(R4)       TEST ANY MARKETS THERE                       
         BZ    RDSP2               NONE - SKIP ENTRY                            
*                                                                               
         LA    R4,L'SPILLTAB(R4)   NEXT TABLE ENTRY                             
         C     R4,=A(SPILLTBX)                                                  
         BL    RDSP2                                                            
         DC    H'0'                                                             
*                                                                               
READSPLX L     R0,=A(SPILLTAB)                                                  
         SR    R4,R0                                                            
         SRDL  R4,32                                                            
         D     R4,=A(L'SPILLTAB)   DIVIDE BY ENTRY LEN                          
         LTR   R4,R4                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R5,SPILLCNT                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        ADD PRODUCTS TO PRDLIST FOR PUTREC TO ADD PASSIVE SPILL PTRS           
*                                                                               
BLDPRDLS NTR1                                                                   
*                                                                               
*        FIND SPILL BUY ELEMS                                                   
*                                                                               
         XC    PRDLST,PRDLST      INIT PRODUCT LIST                             
         XC    DMCB+20(4),DMCB+20                                               
*                                                                               
         L     R6,AREC                                                          
         USING BUYRECD,R6                                                       
*                                                                               
         LA    RF,BDMASPRD         FIRST MASPRD                                 
         BAS   RE,ADDPRDL                                                       
*                                                                               
         LA    RF,1(RF)            SECOND MASPRD                                
         BAS   RE,ADDPRDL                                                       
*                                                                               
         L     R6,AREC             POINT TO BUY RECORED                         
         LA    R6,BDELEM           POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
PPTPRDLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             DONE AT END OF RECORD                        
         BE    PPTPRDDN                                                         
*                                                                               
         CLI   0(R6),X'0B'         LOOKING FOR BUY ELEMENTS                     
         BE    *+8                                                              
         CLI   0(R6),X'0C'                                                      
         BNE   PPTPRDCN                                                         
*                                                                               
         USING REGELEM,R6          ESTABLISH REGULAR POOL BUY ELEMENT           
*                                                                               
*        ADD PRODUCT(S) TO PRDLIST                                              
*                                                                               
         CLI   RLEN,14             SKIP IF NO PRODUCTS ALLOCATED                
         BL    PPTPRDCN                                                         
         LA    RF,10(R6)           POINT TO FIRST PRODUCT                       
         BAS   RE,ADDPRDL                                                       
*                                                                               
         CLI   RLEN,18             SKIP IF NO PIGGYBACK ALLOCATED               
         BL    PPTPRDCN                                                         
*                                                                               
         LA    RF,14(R6)           POINT TO SECOND PRODUCT                      
         BAS   RE,ADDPRDL                                                       
         B     PPTPRDCN                                                         
         SPACE 1                                                                
*================================================================*              
*        ADD PRODUCT IF NOT ALREADY IN LIST                                     
*        RF==> PRODUCT TO ADD TO LIST                                           
*================================================================*              
         SPACE 1                                                                
ADDPRDL  CLI   0(RF),0             TEST NO PRODUCT                              
         BER   RE                  YES - IGNORE                                 
         LA    R1,PRDLST           INIT PRODUCT LIST                            
*                                                                               
ADDPRDL2 CLI   0(R1),0             DONE AT END OF LIST                          
         BE    ADDPRDL4                                                         
         CLC   0(1,R1),0(RF)       SKIP IF PRODUCT IN LIST                      
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         B     ADDPRDL2                                                         
*                                                                               
ADDPRDL4 MVC   0(1,R1),0(RF)       ADD PRODUCT TO LIST                          
         BR    RE                                                               
*                                                                               
PPTPRDCN DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PPTPRDLP                                                         
*                                                                               
PPTPRDDN DS    0H                                                               
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   PPTPRD99                                                         
         CP    PPTCTR,=P'0'                                                     
         BE    PPTPRD99            PRINT ONLY 1ST 20                            
*                                                                               
         MVC   P+5(7),=C'PRDLST='                                               
         GOTO1 HEXOUT,DMCB,PRDLST,P+13,10                                       
         GOTO1 REPORT                                                           
*                                                                               
PPTPRD99 DS    0H                                                               
*                                                                               
         OC    PRDLST,PRDLST       SKIP IF NO PRODUCTS IN LIST                  
         BZ    PPTPRDX                                                          
*                                                                               
         LA    RE,PRDLST           PASS A(PRDLIST) WITH PUTREC                  
         ST    RE,DMCB+20                                                       
*                                                                               
PPTPRDX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
*ADD SPILL PASSIVE POINTERS                                                     
*                                                                               
PUTPTRS  NTR1  LABEL=*                                                          
*                                                                               
*        ADD POOL SPILL PASSIVE POINTERS                                        
*                                                                               
*        FIND SPILL DEMO ELEM                                                   
*                                                                               
         MVC   PPTELCDE,ELCODE     SAVE ELEMENT CODE                            
*                                                                               
         L     R6,AREC             POINT TO BUYREC WITH SPILL                   
         USING BUYRECD,R6                                                       
         MVI   ELCODE,3            LOOKING FOR X'03' DEMO ELEMENT               
         LA    R6,BDELEM           POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   PPTMKTLP                                                         
         CP    PPTCTR,=P'0'                                                     
         BE    PPTMKTLP            PRINT ONLY 1ST 20                            
*                                                                               
         MVC   P+5(20),=CL20'SPILL PASSIVES'                                    
         GOTO1 REPORT                                                           
*                                                                               
PPTMKTLP DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PPTMKTDN                                                         
*                                                                               
         USING NDELEM,R6           ESTABLISH DEMO ELEMENT                       
*                                                                               
*        ADD SPILL PASSIVE FOR POOL                                             
*                                                                               
         L     RE,AREC             POINT TO NEW BUY RECORD                      
         MVC   KEY(10),0(RE)       COPY BUYKEY                                  
         MVC   KEY+4(2),NDPROG     REPLACE MKT WITH AGY SPILL MKT               
         MVI   KEY+10,X'80'        SET SPILL INDICATOR                          
         MVC   KEY+11(1),10(RE)    MOVE LINE NUMBER                             
         MVI   KEY+12,1                                                         
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PPT2                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                        
*                                                                               
         TM    8(R1),X'FF'-X'20'   TEST ALL ERRORS BUT DUP KEY                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        PRINT PASSIVE                                                          
*                                                                               
PPT2     CLI   QOPT1,C'Y'                                                       
         BNE   PPTMKTNP                                                         
         CP    PPTCTR,=P'0'                                                     
         BE    PPTMKTNP            PRINT ONLY 1ST 20                            
*                                                                               
         SR    R7,R7                                                            
         ICM   R7,3,13(R6)                                                      
         AR    R7,R6                                                            
         XC    0(2,R7),0(R7)       SET EOR FLAG                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+40,18                                          
         GOTO1 REPORT                                                           
*                                                                               
         SP    PPTCTR,=P'1'        DECREMENT COUNTER                            
*                                                                               
PPTMKTNP DS    0H                                                               
*                                                                               
PPTMKTCN DS    0H                                                               
*                                                                               
         B     PPTMKTLP                                                         
*                                                                               
PPTMKTDN DS    0H                                                               
*                                                                               
         MVC   ELCODE,PPTELCDE     RESTORE ELEMENT CODE                         
*                                                                               
PUTPTRSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
PPTCTR   DC    PL2'20'                                                          
PPTELCDE DS    X                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
*                                                                               
ELCODE   DS    C                                                                
SPILLCNT DS    F                                                                
SVBINADR DS    A                                                                
ELEM     DS    XL128                                                            
*                                                                               
PRDLST   DS    XL256               PRODUCT LIST                                 
*                                                                               
         DS    0D                                                               
         DC    CL8'**MYBUY'                                                     
MYBUY    DS    4000C                                                            
*                                                                               
         DC    CL8'**MYBUY2'                                                    
MYBUY2   DS    4000C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'SPILLTAB'                                                    
SPILLTAB DS    0XL48                                                            
         DS    48000C                                                           
SPILLTBX EQU   *                                                                
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPFXSPX10/04/00'                                      
         END                                                                    
