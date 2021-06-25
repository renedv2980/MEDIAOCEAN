*          DATA SET PPREPL202  AT LEVEL 030 AS OF 04/01/13                      
*PHASE PPL202A                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE GETUSER                                                                
         TITLE 'PPL202  ESTIMATE SUMMARY'                                       
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*    KWAN 08/00      NEW PBILLREC AND PPBVAL DSECTS                             
*                                                                               
*    BPLA 11/98      CHANGES FOR Y2K                                            
*                                                                               
*    BPLA 8/31/94    NOW LONGER NEED TO USE GETINSA                             
*                    USE GETINS                                                 
*                                                                               
*    BPLA ADD PST DATA                                                          
*                                                                               
PPL202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPL202                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,SPACEND                                                       
         USING PPL2WRKD,R9                                                      
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BE    FXDELE                                                           
*                                                                               
         CLI   MODE,FBUYREQ                                                     
         BE    SCHDTE                                                           
*                                                                               
         CLI   MODE,FBUYCLI                                                     
         BE    SETPROF                                                          
*                                                                               
         CLI   MODE,FBUYEST                                                     
         BE    PEST                                                             
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    PBY                                                              
*                                                                               
         CLI   MODE,LBUYEST                                                     
         BE    RDBILLS                                                          
*                                                                               
         CLI   MODE,LBUYPRO                                                     
         BE    PRDEND                                                           
*                                                                               
         CLI   MODE,FBUYPRO                                                     
         BE    PRDFRST1                                                         
*                                                                               
         CLI   MODE,LBUYCLI                                                     
         BE    CLTEND                                                           
*                                                                               
         CLI   MODE,LBUYREQ                                                     
         BE    REQBK                                                            
*                                                                               
PPL2EX   XMOD1 1                                                                
         EJECT                                                                  
RDBILLS  DS    0H                                                               
         OC    DTELST(2),DTELST                                                 
         BZ    PPL2EX                                                           
*                                                                               
         GOTO1 =A(READB)                                                        
         B     ESTBK                                                            
*                                                                               
PRDEND   GOTO1 =A(PRDBK)                                                        
         B     PPL2EX                                                           
*                                                                               
CLTEND   GOTO1 =A(CLTBK)                                                        
         B     PPL2EX                                                           
*                                                                               
PRDFRST1 MVI   FORCEHED,C'Y'        TO INSURE ONE PRD PER PAGE                  
         LA    R2,PRDTOTS           CLEAR PRODUCT TOTALS                        
         LA    R3,10                                                            
PRDF2    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,PRDF2                                                         
*                                                                               
         LA    R2,PRDGST                                                        
         LA    R3,10                                                            
PRDF4    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,PRDF4                                                         
*                                                                               
         LA    R2,PRDPST                                                        
         LA    R3,10                                                            
PRDF6    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,PRDF6                                                         
*                                                                               
         B     PPL2EX                                                           
*                                  ESTIMATE RECORDS                             
*                                     * SET UP MONTH (YM) LIST FOR              
*                                       ESTIMATE PERIOD                         
*                                     * SET UP MID LINE PRINTING                
*                                                                               
PEST     CLI   FRSTIM,1                                                         
         BE    PPL2EX                                                           
         MVI   FRSTIM,1                                                         
*                                                                               
         MVC   LOWDTE,=3X'FF'                                                   
         XC    HIDTE,HIDTE                                                      
         GOTO1 DATCON,DMCB,(0,PESTST),(3,WORK)                                  
         ZIC   R0,WORK        BINARY YEAR                                       
         ST    R0,FULL                                                          
******   PACK  DUB,PESTST(2)                                                    
******   CVB   R0,DUB                                                           
******   ST    R0,FULL                                                          
         MVC   DTELST(1),FULL+3                                                 
         PACK  DUB,PESTST+2(2)                                                  
         CVB   R0,DUB                                                           
         CH    R0,=H'6'                                                         
         BH    PE00                                                             
         AH    R0,=H'12'                                                        
         IC    R1,DTELST                                                        
         SH    R1,=H'1'                                                         
         STH   R1,HALF                                                          
         MVC   DTELST(1),HALF+1                                                 
PE00     SH    R0,=H'6'                                                         
         ST    R0,FULL                                                          
         MVC   DTELST+1(1),FULL+3                                               
         GOTO1 DATCON,DMCB,(0,PESTEND),(3,WORK)                                 
         ZIC   R0,WORK           BINARY YEAR                                    
         ST    R0,FULL                                                          
******   PACK  DUB,PESTEND(2)                                                   
******   CVB   R0,DUB                                                           
******   ST    R0,FULL                                                          
         PACK  DUB,PESTEND+2(2)                                                 
         CVB   R0,DUB                                                           
         AH    R0,=H'3'                                                         
         CH    R0,=H'12'                                                        
         BNH   PE01                                                             
         SH    R0,=H'12'                                                        
         IC    R1,FULL+3                                                        
         LA    R1,1(R1)                                                         
         ST    R1,FULL                                                          
PE01     STH   R0,HALF                                                          
         MVC   HALF(1),FULL+3                                                   
         LA    R1,DTELST                                                        
         LA    R5,23                                                            
PE02     CLC   0(2,R1),HALF                                                     
         BE    PE20                                                             
         BCT   R5,PE03                                                          
         B     PE20                                                             
PE03     CLI   1(R1),12                                                         
         BNE   PE04                                                             
         IC    R2,0(R1)                                                         
         LA    R2,1(R2)                                                         
         STC   R2,2(R1)                                                         
         MVI   3(R1),1                                                          
         B     PE06                                                             
PE04     IC    R2,1(R1)                                                         
         LA    R2,1(R2)                                                         
         STC   R2,3(R1)                                                         
         MVC   2(1,R1),0(R1)                                                    
PE06     LA    R1,2(R1)                                                         
         B     PE02                                                             
*                                                                               
PE20     MVC   MID1(8),=C'ESTIMATE'                                             
         EDIT  (2,PESTKEST),(3,MID1+9),0,FILL=0                                 
         MVC   MID1+13(L'PESTNAME),PESTNAME                                     
         CLI   PESTNAM2,C' '                                                    
         BNH   *+10                                                             
         MVC   MID2+13(L'PESTNAM2),PESTNAM2                                     
*                                                                               
         GOTO1 DATCON,DMCB,(0,PESTST),(X'20',WORK)                              
         GOTO1 DATCON,DMCB,(0,PESTEND),(X'20',WORK+6)                           
         MVC   MID1+40(2),WORK+2       MM                                       
         MVC   MID1+43(2),WORK+4       DD                                       
         MVC   MID1+46(2),WORK         YY                                       
         MVC   MID1+49(2),WORK+6+2     MM                                       
         MVC   MID1+52(2),WORK+6+4     DD                                       
         MVC   MID1+55(2),WORK+6       YY                                       
         MVI   MID1+42,C'/'                                                     
         MVI   MID1+45,C'/'                                                     
         MVI   MID1+51,C'/'                                                     
         MVI   MID1+54,C'/'                                                     
         MVI   MID1+48,C'-'                                                     
         CLI   PESTSTAT,C'1'       SEE IF LOCKED REG OR PERM                    
         BL    *+10                                                             
         MVC   MID1+59(6),=C'LOCKED'                                            
         GOTO1 =V(GETUSER),DMCB,(C'P',PCLTREC),(C'E',PESTREC),(C':',MIDX        
               1+72),(C':',MID2+72)                                             
         MVI   FORCEMID,C'Y'                                                    
         MVI   ALLOWLIN,5          MUST HAVE AT LEAST 5 LINES                   
         B     PPL2EX                                                           
         EJECT                                                                  
*                                                                               
SETPROF  DS    0H                  READ PROFILES                                
         MVI   PRDACT,C'N'         INITIALIZE ACTIVITY SWITCH                   
         MVI   CLTACT,C'N'         INITIALIZE ACTIVITY SWITCH                   
*                                                                               
         LA    R2,PRDTOTS                                                       
         LA    R3,10                                                            
SETPR02  ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SETPR02                                                       
*                                                                               
         LA    R2,PRDGST                                                        
         LA    R3,10                                                            
SETPR02C ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SETPR02C                                                      
*                                                                               
         LA    R2,PRDPST                                                        
         LA    R3,10                                                            
SETPR02E ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SETPR02E                                                      
*                                                                               
*                                                                               
         LA    R2,CLTTOTS                                                       
         LA    R3,10                                                            
SETPR03  ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SETPR03                                                       
*                                                                               
         LA    R2,CLTGST                                                        
         LA    R3,10                                                            
SETPR03C ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SETPR03C                                                      
*                                                                               
         LA    R2,CLTPST                                                        
         LA    R3,10                                                            
SETPR03D ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SETPR03D                                                      
*                                                                               
         MVI   CDSW,0              SET DEFAULT VALUE                            
*                                  SUBTRACT CD                                  
         OC    PROGPROF,PROGPROF   CHK FOR PROFILE                              
         BZ    SETPRX                                                           
         CLI   PROGPROF,C'N'       SEE IF SUBTRACTING CD                        
         BNE   *+8                                                              
         MVI   CDSW,1                                                           
         MVI   OLDSW,0                                                          
         CLI   PROGPROF+1,C'4'     SEE WHERE TO INCLUDE OLD BILLING             
         BL    *+10                                                             
         MVC   OLDSW,PROGPROF+1                                                 
*                                                                               
SETPRX   B     PPL2EX                                                           
         EJECT                                                                  
*                                  SET UP KEY AND READ BILL RECS                
*                                                                               
*                                  ADD TO ACCUMS ON BASIS OF                    
*                                  BILLABLE DATE AND INSERION DATE              
*                                                                               
PBY      DS    0H                                                               
*                                                                               
         GOTO1 GETINS,DMCB,PBUYREC,(0,GROSS),PPRDKPRD,0,=C'PST'                 
         L     R1,DMCB+16                                                       
         USING GVALUES,R1                                                       
         ICM   R0,15,GSTTAXPD                                                   
         CVD   R0,DUB                                                           
         AP    ESTGST+32(8),DUB                                                 
*                                                                               
         LA    RE,PSTAREA                                                       
PBY0     CLC   0(2,RE),=C'  '                                                   
         BNH   PBY0X                                                            
         ICM   R0,15,PSTTAXPD-PSTAREA(RE)                                       
         CVD   R0,DUB                                                           
         AP    ESTPST+32(8),DUB                                                 
         LA    RE,PSTAREAL(RE)                                                  
         B     PBY0                                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
PBY0X    DS    0H                                                               
         TM    PBUYCNTL,X'80'      TEST FOR A DELETE                            
         BZ    PBY01                                                            
         XC    GROSS(20),GROSS                                                  
*                                  BILLABLE DATE                                
PBY01    DS    0H                                                               
         MVC   BILLDT,PBDBDATE                                                  
         MVC   INSDTE,PBUYKDAT                                                  
*                                  SAVE LOWEST BUY DATE (INS OR BILL)           
         CLC   LOWDTE,PBDBDATE                                                  
         BNH   *+10                                                             
         MVC   LOWDTE,PBDBDATE                                                  
         CLC   LOWDTE,PBUYKDAT                                                  
         BNH   *+10                                                             
         MVC   LOWDTE,PBUYKDAT                                                  
*                                                                               
         CLC   HIDTE,PBDBDATE                                                   
         BNL   *+10                                                             
         MVC   HIDTE,PBDBDATE                                                   
         CLC   HIDTE,PBUYKDAT                                                   
         BNL   *+10                                                             
         MVC   HIDTE,PBUYKDAT                                                   
*                                                                               
*                                  INSERTION DATE GROSS                         
*                                                                               
         LA    R2,DTELST                                                        
         LA    R3,ACCUM                                                         
         LA    R4,1                                                             
PBY02    CLI   0(R2),0                                                          
         BNE   PBY03                                                            
         TM    PBUYCNTL,X'80'                                                   
         BNZ   PPL2EX                                                           
         DC    H'0'                                                             
PBY03    DS    0H                                                               
         CLC   0(2,R2),INSDTE                                                   
         BE    PBY04                                                            
         LA    R4,1(R4)                                                         
         LA    R2,2(R2)                                                         
         B     PBY02                                                            
PBY04    MH    R4,=H'88'                                                        
         SH    R4,=H'88'                                                        
         AR    R3,R4                                                            
         MVC   DUB,0(R3)                                                        
         ICM   R4,15,GROSS                                                      
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY05                                                            
         ICM   R4,15,AGYCOM                                                     
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY05    MVC   0(8,R3),DUB                                                      
*                                  BILLED MONTH GROSS                           
PBY10    LA    R2,DTELST                                                        
         LA    R3,ACCUM                                                         
         LA    R4,1                                                             
PBY12    CLI   0(R2),0                                                          
         BNE   PBY13                                                            
         CLC   QAGENCY,=C'JW'      NO BILL MONTH SUMMARY FOR JW                 
         BE    PBY12C                                                           
         DC    H'0'                                                             
*                                                                               
PBY12C   SH    R2,=H'2'            BACK-UP TO LAST MONTH                        
         SH    R4,=H'1'                                                         
         B     PBY14               AND POST THERE                               
*                                                                               
PBY13    CLC   0(2,R2),BILLDT                                                   
         BE    PBY14                                                            
         LA    R4,1(R4)                                                         
         LA    R2,2(R2)                                                         
         B     PBY12                                                            
PBY14    MH    R4,=H'88'                                                        
         SH    R4,=H'88'                                                        
         AR    R3,R4                                                            
         MVC   DUB,8(R3)                                                        
         ICM   R4,15,GROSS                                                      
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY15                                                            
         ICM   R4,15,AGYCOM                                                     
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY15    MVC   8(8,R3),DUB                                                      
*                                  CASH DISCOUNT                                
         ICM   R4,15,CSHDSC                                                     
         CVD   R4,DUB                                                           
         AP    16(8,R3),DUB                                                     
*                                  GROSS LESS CD                                
         MVC   DUB,24(R3)                                                       
         ICM   R4,15,GROSS                                                      
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY16                                                            
         ICM   R4,15,AGYCOM                                                     
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY16    ICM   R4,15,CSHDSC                                                     
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
         MVC   24(8,R3),DUB                                                     
*                                  PAID                                         
         MVC   DUB,32(R3)                                                       
         ICM   R4,15,PGROSS                                                     
         CVD   R4,DOUBLE                                                        
         AP    DUB,DOUBLE                                                       
         CLI   NETSW,1             SEE IF SHOWING NET                           
         BNE   PBY17                                                            
         ICM   R4,15,PAGYCOM       YES - SUBTRACT AGYCOM                        
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY17    CLI   CDSW,1              SEE IF SUBTRACTING CD                        
         BE    PBY18               NO                                           
         ICM   R4,15,PCSHDSC                                                    
         CVD   R4,DOUBLE                                                        
         SP    DUB,DOUBLE                                                       
PBY18    MVC   32(8,R3),DUB                                                     
         B     PPL2EX                                                           
         EJECT                                                                  
*                                  ESTIMATE BREAK                               
*                                  PRINT MONTHLY DETAIL LINES                   
*                                  ACCUM TOTALS & PRINT                         
ESTBK    LA    R2,DTELST                                                        
         LA    R3,ACCUM                                                         
         LA    R6,23                                                            
EB02     LR    R7,R3                                                            
         CLI   0(R2),0                                                          
         BE    EB10                                                             
*                                                                               
         ST    R3,FULL                                                          
         LA    RE,10                                                            
EB02AA   CP    0(8,R3),=P'0'                                                    
         BNE   EB02AB                                                           
         LA    R3,8(R3)                                                         
         BCT   RE,EB02AA                                                        
         OC    0(4,R3),0(R3)                                                    
         BNZ   EB02AB                                                           
         L     R3,FULL                                                          
         LA    R3,88(R3)                                                        
         B     EB09                                                             
*                                                                               
EB02AB   L     R3,FULL                                                          
*                                                                               
EB02A    XC    P(132),P                                                         
         XR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         BAS   R8,PCVD                                                          
         MVC   DATE(2),DUB+6                                                    
         IC    R0,1(R2)                                                         
         BAS   R8,PCVD                                                          
         MVC   DATE+2(2),DUB+6                                                  
         MVC   DATE+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,DATE,(X'09',P)                                       
*                                                                               
EB03     DS    0H                                                               
         EDIT  (P8,0(R3)),(12,P+7),2,FLOAT=-                                    
         EDIT  (P8,8(R3)),(12,P+20),2,FLOAT=-                                   
         EDIT  (P8,16(R3)),(9,P+33),2,FLOAT=-                                   
         EDIT  (P8,24(R3)),(12,P+43),2,FLOAT=-                                  
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
         CLI   80(R3),X'01'              B4 BILLING DONE                        
         BE    EB03B                                                            
         CP    40(8,R3),=P'0'         STILL CHECK FOR $'S                       
         BE    EB03D                                                            
EB03B    EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
*                                                                               
EB03D    CLI   81(R3),X'01'           B5 BILLING DONE                           
         BE    EB03F                                                            
         CP    48(8,R3),=P'0'         STILL CHECK FOR $'S                       
         BE    EB03H                                                            
EB03F    EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
*                                                                               
EB03H    CLI   82(R3),X'01'           B6 BILLING DONE                           
         BE    EB03J                                                            
         CP    56(8,R3),=P'0'         STILL CHECK FOR $'S                       
         BE    EB03L                                                            
EB03J    EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
*                                                                               
EB03L    DS    0H                                                               
         CLI   83(R3),X'01'           B7 BILLING DONE                           
         BE    EB03N                                                            
         CP    64(8,R3),=P'0'         STILL CHECK FOR $'S                       
         BE    EB03P                                                            
EB03N    EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
EB03P    DS     0H                                                              
*                                                                               
         MVC   DUB,24(R3)                                                       
         CLI   CDSW,0              SEE IF SUBTRACTING CD                        
         BE    *+10                YES                                          
         MVC   DUB,8(R3)           ELSE USE GROSS (BILL MTH)                    
         SP    DUB,40(8,R3)                                                     
         SP    DUB,48(8,R3)                                                     
         SP    DUB,56(8,R3)                                                     
         SP    DUB,64(8,R3)                                                     
         MVC   72(8,R3),DUB                                                     
         EDIT  (P8,72(R3)),(12,P+120),2,FLOAT=-                                 
         LA    R4,ACCUM+TOTDSP     23X88                                        
         CR    R3,R4                                                            
         BE    EB08A               SEE IF DOING ESTIMATE TOTALS                 
*                                  IF SO ROLL TO REQUEST TOTALS                 
         LA    R5,10                                                            
EB08     AP    0(8,R4),0(8,R3)                                                  
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R5,EB08                                                          
*                                                                               
         OC    0(4,R4),0(R3)       OR BBILLING TYPE BYTES INTO TOTALS           
*                                                                               
         LA    R3,8(R3)            BUMP PAST BILLING TYPE BYTES                 
         B     EB08X                                                            
*                                                                               
EB08A    DS    0H                                                               
         MVC   HEAD4+48(22),STHD                                                
         MVC   HEAD3+49(20),TDHD                                                
         MVI   RCSUBPRG,0                                                       
         CLI   NETSW,1             SEE IF SHOWING NET CLEARED + BILLED          
         BNE   *+8                                                              
         MVI   RCSUBPRG,10                                                      
*                                                                               
*        (40) INSTEAD OF 44 SO I WON'T CHECK BILLING BITS                       
*                                                                               
         LA    RF,10                                                            
         LA    RE,ESTGST                                                        
         CP    0(8,RE),=P'0'                                                    
         BNE   EB08A5              NO GST - LEAVE SPACING AT 2                  
         LA    RE,8(RE)                                                         
         BCT   RF,*-14                                                          
*                                                                               
         LA    RF,10                                                            
         LA    RE,ESTPST                                                        
         CP    0(8,RE),=P'0'                                                    
         BNE   EB08A5                                                           
         LA    RE,8(RE)                                                         
         BCT   RF,*-14                                                          
         B     EB08A6              AND NO PST - LEAVE SPACING AT 2              
*                                                                               
EB08A5   MVI   SPACING,1                                                        
EB08A6   GOTO1 REPORT                                                           
         LA    R3,ESTGST           POINT TO EST GST ACCUMS                      
*                                                                               
*        (40) INSTEAD OF 44 SO I WON'T CHECK BILLING BITS                       
*                                                                               
         LA    RF,10                                                            
         LA    RE,ESTGST                                                        
         CP    0(8,RE),=P'0'                                                    
         BNE   EB08A7                                                           
         LA    RE,8(RE)                                                         
         BCT   RF,*-14                                                          
         B     EB08P               NO GST                                       
*                                                                               
EB08A7   MVC   P+1(3),=C'GST'                                                   
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
*                                                                               
         CLI   80(R3),X'01'        B4 BILLING DONE                              
         BE    EB08B                                                            
         CP    40(8,R3),=P'0'      STILL CHECK FOR $'S                          
         BE    EB08D                                                            
*                                                                               
EB08B    EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
*                                                                               
EB08D    CLI   81(R3),X'01'        B5 BILLING DONE                              
         BE    EB08F                                                            
         CP    48(8,R3),=P'0'      STILL CHECK FOR $'S                          
         BE    EB08H                                                            
EB08F    EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
*                                                                               
EB08H    CLI   82(R3),X'01'        B6 BILLING DONE                              
         BE    EB08J                                                            
         CP    56(8,R3),=P'0'      STILL CHECK FOR $'S                          
         BE    EB08L                                                            
EB08J    EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
*                                                                               
EB08L    DS    0H                                                               
         CLI   83(R3),X'01'        B7 BILLING DONE                              
         BE    EB08N                                                            
         CP    64(8,R3),=P'0'      STILL CHECK FOR $'S                          
         BE    EB08P                                                            
*                                                                               
EB08N    EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
EB08P    DS     0H                 TRY FOR PST                                  
*                                                                               
         MVI   SPACING,2                                                        
         LA    RF,10                                                            
         LA    RE,ESTPST                                                        
         CP    0(8,RE),=P'0'                                                    
         BNE   *+16                                                             
         LA    RE,8(RE)                                                         
         BCT   RF,*-14                                                          
         B     *+8                                                              
         MVI   SPACING,1                                                        
*                                                                               
         MVC   HEAD4+48(22),STHD                                                
         MVC   HEAD3+49(20),TDHD                                                
         MVI   RCSUBPRG,0                                                       
         CLI   NETSW,1             SEE IF SHOWING NET CLEARED + BILLED          
         BNE   *+8                                                              
         MVI   RCSUBPRG,10                                                      
         GOTO1 REPORT                                                           
*                                                                               
         LA    RF,10                                                            
         LA    RE,ESTPST                                                        
         CP    0(8,RE),=P'0'                                                    
         BNE   *+16                                                             
         LA    RE,8(RE)                                                         
         BCT   RF,*-14                                                          
         B     EB09                                                             
*                                                                               
         LA    R3,ESTPST            POINT TO EST PST ACCUMS                     
*                                                                               
         MVC   P+1(3),=C'PST'                                                   
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
         CLI   80(R3),X'01'              B4 BILLING DONE                        
         BE    EB08Q                                                            
         CP    40(8,R3),=P'0'         STILL CHECK FOR $'S                       
         BE    EB08R                                                            
EB08Q    EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
*                                                                               
EB08R    CLI   81(R3),X'01'           B5 BILLING DONE                           
         BE    EB08S                                                            
         CP    48(8,R3),=P'0'         STILL CHECK FOR $'S                       
         BE    EB08T                                                            
EB08S    EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
*                                                                               
EB08T    CLI   82(R3),X'01'           B6 BILLING DONE                           
         BE    EB08U                                                            
         CP    56(8,R3),=P'0'         STILL CHECK FOR $'S                       
         BE    EB08V                                                            
EB08U    EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
*                                                                               
EB08V    DS    0H                                                               
         CLI   83(R3),X'01'           B7 BILLING DONE                           
         BE    EB08V5                                                           
         CP    64(8,R3),=P'0'         STILL CHECK FOR $'S                       
         BE    EB08W                                                            
EB08V5   EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
EB08W    DS     0H                                                              
         MVI   SPACING,2                                                        
*                                                                               
EB08X    MVC   HEAD4+48(22),STHD                                                
         MVC   HEAD3+49(20),TDHD                                                
         MVI   RCSUBPRG,0                                                       
         CLI   NETSW,1             SEE IF SHOWING NET CLEARED + BILLED          
         BNE   *+8                                                              
         MVI   RCSUBPRG,10                                                      
         GOTO1 REPORT                                                           
*                                                                               
EB09     LA    R2,2(R2)                                                         
         BCT   R6,EB02                                                          
EB10     CLI   SW,1                                                             
         BE    EBEX                                                             
         MVI   SW,1                                                             
         LA    R3,ACCUM+TOTDSP     23X88                                        
         MVC   P+1(5),=C'TOTAL'                                                 
         LA    R6,1                                                             
         MVI   SPACING,2                                                        
         B     EB03                                                             
*                                                                               
EBEX     DS    0H                  ROLL EST TOTALS TO PRODUCT TOTALS            
         LA    R2,ACCUM+TOTDSP     POINT TO TOTALS                              
         LA    R4,PRDTOTS                                                       
         LA    R3,10                                                            
EBEX5    AP    0(8,R4),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,EBEX5                                                         
*                                                                               
*        ROLL ESTGST TO CLTGST                                                  
*                                                                               
         LA    R2,ESTGST                                                        
         LA    R4,PRDGST                                                        
         LA    R3,10                                                            
EBEX10   AP    0(8,R4),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,EBEX10                                                        
*                                                                               
*        ROLL ESTPST TO CLTPST                                                  
*                                                                               
         LA    R2,ESTPST                                                        
         LA    R4,PRDPST                                                        
         LA    R3,10                                                            
EBEX12   AP    0(8,R4),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,EBEX12                                                        
*                                                                               
EBEX20   DS    0H                  ROLL EST TOTALS CLIENT TOTALS                
         LA    R2,ACCUM+TOTDSP     POINT TO TOTALS                              
         LA    R4,CLTTOTS                                                       
         LA    R3,10                                                            
EBEC     AP    0(8,R4),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,EBEC                                                          
*                                                                               
*        ROLL ESTGST TO CLTGST                                                  
*                                                                               
         LA    R2,ESTGST                                                        
         LA    R4,CLTGST                                                        
         LA    R3,10                                                            
EBEE     AP    0(8,R4),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,EBEE                                                          
*                                                                               
*        ROLL ESTPST TO CLTPST                                                  
*                                                                               
         LA    R2,ESTPST                                                        
         LA    R4,CLTPST                                                        
         LA    R3,10                                                            
EBEG     AP    0(8,R4),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,EBEG                                                          
*                                                                               
         LA    R2,ACCUM            CLEAR ACCUMULATOR TABLE                      
         LA    R3,264              264 SETS OF PL8                              
EBXP     ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,EBXP                                                          
*                                                                               
         XC    ESTDATA,ESTDATA                                                  
*                                                                               
         LA    RF,11                                                            
         LA    RE,ESTGST                                                        
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,*-10                                                          
*                                                                               
         LA    RF,11                                                            
         LA    RE,ESTPST                                                        
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,*-10                                                          
*                                                                               
         B     PPL2EX                                                           
         EJECT                                                                  
*                                                                               
*        ON RUN FIRST, TURN ON DMBITS TO HAVE DELETED RECORDS PASSED            
*                                                                               
FXDELE   OI    DMINBTS,X'08'                                                    
         OI    DMOUTBTS,X'FD'                                                   
         B     PPL2EX                                                           
*                                                                               
*        START OF REQUEST                                                       
*        SET UP SCHEDULE DATES IF EST = ALL                                     
*                                                                               
SCHDTE   DS    0H                                                               
         MVI   NETSW,0             DEFAULT IS GROSS                             
         CLI   QOPT1,C'N'                                                       
         BNE   *+8                                                              
         MVI   NETSW,1             SET TO SHOW NET                              
*                                                                               
         XC    DTELST(WKX-DTELST),DTELST                                        
*                                                                               
         LA    R2,ACCUM            CLEAR ACCUMULATOR TABLE                      
         LA    R3,264              264 SETS OF PL8                              
SD02     ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SD02                                                          
*                                                                               
         LA    RF,11                                                            
         LA    RE,ESTGST                                                        
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,*-10                                                          
*                                                                               
         LA    RF,11                                                            
         LA    RE,ESTPST                                                        
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,*-10                                                          
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   QSTART,C'0'                                                      
         BL    PPL2EX                                                           
         MVC   STHD+9(4),=C'THRU'                                               
         GOTO1 DATCON,DMCB,QSTART,(X'05',STHD)                                  
         GOTO1 DATCON,DMCB,QEND,(X'05',STHD+14)                                 
         MVC   HEAD4+48(22),STHD                                                
         CLI   QBPDATE,C' '                                                     
         BE    SD04                                                             
         MVC   TDHD,=CL20'** BILLABLE DATES **'                                 
         CLI   QBPDATE,C'B'                                                     
         BE    SD04                                                             
         MVC   TDHD,=CL20'** PAYABLE DATES **'                                  
SD04     DS    0H                                                               
         B     PPL2EX                                                           
*                                                                               
*        END OF REQUEST, SKIP TO NEW PAGE                                       
*                                                                               
REQBK    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     PPL2EX                                                           
*                                                                               
*        CONVERT TO DECIMAL                                                     
*                                                                               
PCVD     CVD   R0,WORK                                                          
         UNPK  DUB,WORK(8)                                                      
         OI    DUB+7,X'F0'                                                      
         BR    R8                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
READB    CSECT                     READ BILLS                                   
         NMOD1 0,READB                                                          
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,SPACEND                                                       
         USING PPL2WRKD,R9                                                      
*                                  DO PRODUCT TOTALS                            
         MVC   TYPTAB,=C'4567'                                                  
*                                                                               
         CLI   DTELST,0            WILL BE NON-ZERO IF I GOT FBUYEST            
         BE    READBX              NO EST ACTIVITY                              
         MVI   PRDACT,C'Y'         SET ACTIVITY                                 
         MVI   CLTACT,C'Y'         SET ACTIVITY                                 
         MVC   SVKEYS(64),KEY      SAVE KEY AND KEYSAVE                         
         XC    KEY,KEY                                                          
         MVC   KEY(7),RCSVAGY                                                   
         CLC   QPRODUCT,=C'ZZZ'    NO PROD IF ZZZ                               
         BE    *+10                                                             
         MVC   KEY+7(3),PPRDKPRD                                                
         MVI   KEY+3,X'08'                                                      
         MVC   KEY+10(2),PESTKEST                                               
         BAS   R8,GETHI                                                         
         B     *+8                                                              
RB02     BAS   R8,GETSEQ                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BNE   RBEX                                                             
         OC    KEYSAVE+7(3),KEYSAVE+7                                           
         BZ    *+14                                                             
         CLC   KEY+7(3),KEYSAVE+7       PRODUCT                                 
         BNE   RB02                                                             
         CLC   KEY+10(2),KEYSAVE+10     ESTIMATE                                
         BNE   RB02                                                             
         LA    R2,BQSTART          USE BQSTART + BQEND                          
         CLI   QBPDATE,C'B'        IF BILL DATE REQ                             
         BE    *+8                                                              
         LA    R2,LOWDTE           OR LOWDTE AND HIDTE                          
         CLC   KEY+12(2),0(R2)      AS BILL DATE FILTERS                        
         BL    RB02                                                             
         CLC   KEY+12(2),3(R2)                                                  
         BH    RB02                                                             
*                                                                               
         BAS   R8,GETBILL1                                                      
         B     PBL                                                              
*                                                                               
RBEX     MVC   KEY(64),SVKEYS                                                   
         B     READBX                                                           
*                                  READ HI                                      
GETHI    LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     GETIT                                                            
*                                  READ SEQ                                     
GETSEQ   LA    RF,DMRSEQ                                                        
*                                                                               
GETIT    ST    RF,DMCB                                                          
         LR    R0,R8                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
*                                                                               
CHCKIT   TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R8,R0                                                            
         BR    R8                                                               
*                                                                               
GETBILL1 LR    R0,R8                                                            
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,PBILLREC,DMWORK               
         B     CHCKIT                                                           
*                                                                               
*                                  BILLING RECORDS                              
*                                                                               
PBL      DS    0H                                                               
         CLI   PBILLTYP,C'B'                                                    
         BE    PB01                WHAT IF NOT 'B' TYPE?                        
         CLI   PBILLTYP,C'M'                                                    
         BE    PB01                                                             
         CLI   OLDSW,0                                                          
         BE    RB02                NO INCLUDING OLD BILLING                     
         MVC   PBILLMOD,OLDSW      SET WHERE TO ACCUMLATE B4-B7                 
         B     PB01                                                             
*                                                                               
PB01     LA    R2,ACCUM                                                         
         LA    R3,DTELST                                                        
         LA    R4,1                                                             
PB02     CLI   0(R3),0                                                          
         BE    RB02                                                             
         CLC   0(2,R3),PBILKMOS                                                 
         BE    PB04                                                             
         LA    R4,1(R4)                                                         
         LA    R3,2(R3)                                                         
         B     PB02                                                             
PB04     MH    R4,=H'88'                                                        
         SH    R4,=H'88'                                                        
         AR    R2,R4                                                            
         MVC   DUB,0(R2)                                                        
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*                                                                               
         MVC   PBILLGRS,PPBVEBG    SET "EFFECTIVE" VALUES INTO BILLREC          
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
         ZAP   WKEFFCD,PPBVEBC     "EFFECTIVE" CD                               
*                                                                               
*        DETAIL BILLED                                                          
*                                                                               
BLLB4    DS    0H                                                               
         CLI   PBILLMOD,C'4'                                                    
         BNE   BLLB5                                                            
*                                                                               
         MVC   DUB,40(R2)                                                       
         BAS   RE,BLTPAMT                                                       
         MVC   40(8,R2),DUB                                                     
         OI    80(R2),X'01'                                                     
*                                                                               
         ICM   R0,15,PPBVGST                                                    
         CVD   R0,DUB                                                           
         AP    ESTGST+40(8),DUB                                                 
         OI    ESTGST+80,X'01'                                                  
*                                                                               
         ICM   R0,15,PPBVPST                                                    
         CVD   R0,DUB                                                           
         AP    ESTPST+40(8),DUB                                                 
         OI    ESTPST+80,X'01'                                                  
*                                                                               
         B     RB02                                                             
*                                                                               
BLLB5    CLI   PBILLMOD,C'5'                                                    
         BNE   BLLB6                                                            
*                                                                               
         MVC   DUB,48(R2)                                                       
         BAS   RE,BLTPAMT                                                       
         MVC   48(8,R2),DUB                                                     
         OI    81(R2),X'01'                                                     
*                                                                               
         ICM   R0,15,PPBVGST                                                    
         CVD   R0,DUB                                                           
         AP    ESTGST+48(8),DUB                                                 
         OI    ESTGST+81,X'01'                                                  
*                                                                               
         ICM   R0,15,PPBVPST                                                    
         CVD   R0,DUB                                                           
         AP    ESTPST+48(8),DUB                                                 
         OI    ESTPST+81,X'01'                                                  
*                                                                               
         B     RB02                                                             
*                                                                               
BLLB6    CLI   PBILLMOD,C'6'                                                    
         BNE   BLLB7                                                            
*                                                                               
         MVC   DUB,56(R2)                                                       
         BAS   RE,BLTPAMT                                                       
         MVC   56(8,R2),DUB                                                     
         OI    82(R2),X'01'                                                     
*                                                                               
         ICM   R0,15,PPBVGST                                                    
         CVD   R0,DUB                                                           
         AP    ESTGST+56(8),DUB                                                 
         OI    ESTGST+82,X'01'                                                  
*                                                                               
         ICM   R0,15,PPBVPST                                                    
         CVD   R0,DUB                                                           
         AP    ESTPST+56(8),DUB                                                 
         OI    ESTPST+82,X'01'                                                  
*                                                                               
         B     RB02                                                             
*                                                                               
BLLB7    CLI   PBILLMOD,C'7'                                                    
         BE    BLLB7A                 IS B BILLTYPE BUT NOT 4,5,6,7             
         DC    H'0'                                                             
*                                                                               
BLLB7A   MVC   DUB,64(R2)                                                       
         BAS   RE,BLTPAMT                                                       
         MVC   64(8,R2),DUB                                                     
         OI    83(R2),X'01'                                                     
*                                                                               
         ICM   R0,15,PPBVGST                                                    
         CVD   R0,DUB                                                           
         AP    ESTGST+64(8),DUB                                                 
         OI    ESTGST+83,X'01'                                                  
*                                                                               
         ICM   R0,15,PPBVPST                                                    
         CVD   R0,DUB                                                           
         AP    ESTPST+64(8),DUB                                                 
         OI    ESTPST+83,X'01'                                                  
*                                                                               
         B     RB02                                                             
*                                                                               
*                                                                               
*                                                                               
BLTPAMT  DS    0H                                                               
         CLI   NETSW,1                                                          
         BNE   BLTP5                                                            
         ZAP   DOUBLE,PBILLNET     USING NET/NET                                
         AP    DUB,DOUBLE          ADD G-AC-CD TO ACCUMULATORS                  
         CLI   CDSW,0              SEE IF SUBTRACTING CD                        
         BER   RE                  IF NOT EXIT BACK                             
         AP    DUB,WKEFFCD         SUBTRACT CD                                  
         BR    RE                                                               
BLTP5    ZAP   DOUBLE,PBILLBIL     USING GROSS-CD                               
         AP    DUB,DOUBLE                                                       
         CLI   CDSW,0              SEE IF SUBTRACTING CD                        
         BER   RE                  IF NOT EXIT BACK                             
         AP    DUB,WKEFFCD         SUBTRACT CD                                  
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
READBX   XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                  PRODUCT BREAK                                
PRDBK    CSECT                                                                  
         NMOD1 0,PRDBK                                                          
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,SPACEND                                                       
         USING PPL2WRKD,R9                                                      
*                                                                               
*        DO PRODUCT TOTALS                                                      
*                                                                               
         CLI   PRDACT,C'Y'                                                      
         BNE   PRDBX                                                            
*                                                                               
         MVI   ALLOWLIN,4                                                       
*                                                                               
         MVC   P+1(7),=C'PRODUCT'                                               
*                                                                               
         MVC   HEAD4+48(22),STHD                                                
         MVC   HEAD3+49(20),TDHD                                                
         MVI   RCSUBPRG,0                                                       
         CLI   NETSW,1             SEE IF SHOWING NET CLEARED + BILLED          
         BNE   *+8                                                              
         MVI   RCSUBPRG,10                                                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+1(5),=C'TOTAL'                                                 
         LA    R3,PRDTOTS                                                       
         EDIT  (P8,0(R3)),(12,P+7),2,FLOAT=-                                    
         EDIT  (P8,8(R3)),(12,P+20),2,FLOAT=-                                   
         EDIT  (P8,16(R3)),(9,P+33),2,FLOAT=-                                   
         EDIT  (P8,24(R3)),(12,P+43),2,FLOAT=-                                  
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
         EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
         EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
         EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
         EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
         LA    R4,24(R3)                                                        
         CLI   CDSW,0              SEE IF SUBTRACTING CD                        
         BE    *+8                 YES                                          
         LA    R4,8(R3)            ELSE USE GROSS (BILL MTH)                    
         ZAP   DUB,0(8,R4)                                                      
         SP    DUB,40(8,R3)                                                     
         SP    DUB,48(8,R3)                                                     
         SP    DUB,56(8,R3)                                                     
         SP    DUB,64(8,R3)                                                     
         ZAP   72(8,R3),DUB                                                     
         EDIT  (P8,72(R3)),(12,P+120),2,FLOAT=-                                 
*                                                                               
         MVC   HEAD4+48(22),STHD                                                
         MVC   HEAD3+49(20),TDHD                                                
         MVI   RCSUBPRG,0                                                       
         CLI   NETSW,1             SEE IF SHOWING NET CLEARED + BILLED          
         BNE   *+8                                                              
         MVI   RCSUBPRG,10                                                      
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,PRDGST           POINT TO EST GST ACCUMS                      
         LA    R4,10                                                            
PRDBK3   CP    0(8,R3),=P'0'                                                    
         BNE   PRDBK5                                                           
         LA    R3,8(R3)                                                         
         BCT   R4,PRDBK3                                                        
         B     PRDBK10                                                          
*                                                                               
PRDBK5   MVC   P+1(3),=C'GST'                                                   
         LA    R3,PRDGST            POINT TO EST GST ACCUMS                     
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
         EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
         EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
         EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
         EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
PRDBK10  DS    0H                                                               
*                                                                               
         LA    R3,PRDPST            POINT TO PRD PST ACCUMS                     
         LA    R4,10                                                            
PRDBK12  CP    0(8,R3),=P'0'                                                    
         BNE   PRDBK14                                                          
         LA    R3,8(R3)                                                         
         BCT   R4,PRDBK12                                                       
         B     PRDBK18                                                          
*                                                                               
PRDBK14  MVC   P+1(3),=C'PST'                                                   
         LA    R3,PRDPST            POINT TO PRD PST ACCUMS                     
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
         EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
         EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
         EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
         EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
PRDBK18  DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(65),=C'** CASH DISCOUNT IS SUBTRACTED FROM BILLED AND X        
               CLEARED AMOUNTS **'                                              
         CLI   CDSW,0                                                           
         BE    PRDBK20                                                          
*                                                                               
         MVC   P(69),=C'** CASH DISCOUNT IS NOT SUBTRACTED FROM BILLED X        
               AND CLEARED AMOUNTS **'                                          
PRDBK20  GOTO1 REPORT                                                           
*                                                                               
*                                                                               
PRDBKX   DS    0H                                                               
PRDBX    LA    R2,ACCUM                                                         
         LA    R3,264              24 SETS OF 11PL8                             
PRDBXA   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,PRDBXA                                                        
*                                                                               
         XC    ESTDATA,ESTDATA                                                  
*                                                                               
         LA    RF,11                                                            
         LA    RE,ESTGST                                                        
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,*-10                                                          
*                                                                               
         LA    RF,11                                                            
         LA    RE,ESTPST                                                        
         ZAP   0(8,RE),=P'0'                                                    
         LA    RE,8(RE)                                                         
         BCT   RF,*-10                                                          
*                                                                               
         MVI   PRDACT,C'N'                                                      
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLTBK    CSECT                                                                  
         NMOD1 0,CLTBK                                                          
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R9,SPACEND                                                       
         USING PPL2WRKD,R9                                                      
*                                  DO CLIENT TOTALS                             
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTBKX                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(6),=C'CLIENT'                                                
*                                                                               
         MVC   HEAD4+48(22),STHD                                                
         MVC   HEAD3+49(20),TDHD                                                
         MVI   RCSUBPRG,20                                                      
         CLI   NETSW,1             SEE IF SHOWING NET CLEARED + BILLED          
         BNE   *+8                                                              
         MVI   RCSUBPRG,30                                                      
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+1(5),=C'TOTAL'                                                 
         LA    R3,CLTTOTS                                                       
         EDIT  (P8,0(R3)),(12,P+7),2,FLOAT=-                                    
         EDIT  (P8,8(R3)),(12,P+20),2,FLOAT=-                                   
         EDIT  (P8,16(R3)),(9,P+33),2,FLOAT=-                                   
         EDIT  (P8,24(R3)),(12,P+43),2,FLOAT=-                                  
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
         EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
         EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
         EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
         EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
         LA    R4,24(R3)                                                        
         CLI   CDSW,0              SEE IF SUBTRACTING CD                        
         BE    *+8                 YES                                          
         LA    R4,8(R3)            ELSE USE GROSS (BILL MTH)                    
         ZAP   DUB,0(8,R4)                                                      
         SP    DUB,40(8,R3)                                                     
         SP    DUB,48(8,R3)                                                     
         SP    DUB,56(8,R3)                                                     
         SP    DUB,64(8,R3)                                                     
         ZAP   72(8,R3),DUB                                                     
         EDIT  (P8,72(R3)),(12,P+120),2,FLOAT=-                                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,CLTGST            POINT TO EST GST ACCUMS                     
         LA    R4,10                                                            
CLTBK3   CP    0(8,R3),=P'0'                                                    
         BNE   CLTBK5                                                           
         LA    R3,8(R3)                                                         
         BCT   R4,CLTBK3                                                        
         B     CLTBK5X                                                          
*                                                                               
CLTBK5   MVC   P+1(3),=C'GST'                                                   
         LA    R3,CLTGST            POINT TO EST GST ACCUMS                     
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
         EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
         EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
         EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
         EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
CLTBK5X  DS    0H                                                               
         LA    R3,CLTPST            POINT TO EST PST ACCUMS                     
         LA    R4,10                                                            
CLTBK8   CP    0(8,R3),=P'0'                                                    
         BNE   CLTBK9                                                           
         LA    R3,8(R3)                                                         
         BCT   R4,CLTBK8                                                        
         B     CLTBK20                                                          
*                                                                               
CLTBK9   MVC   P+1(3),=C'PST'                                                   
         LA    R3,CLTPST            POINT TO CLT PST ACCUMS                     
         EDIT  (P8,32(R3)),(12,P+56),2,FLOAT=-                                  
         EDIT  (P8,40(R3)),(12,P+69),2,FLOAT=-                                  
         EDIT  (P8,48(R3)),(12,P+82),2,FLOAT=-                                  
         EDIT  (P8,56(R3)),(12,P+95),2,FLOAT=-                                  
         EDIT  (P8,64(R3)),(12,P+108),2,FLOAT=-                                 
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
CLTBK20  DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(65),=C'** CASH DISCOUNT IS SUBTRACTED FROM BILLED AND X        
               CLEARED AMOUNTS **'                                              
         CLI   CDSW,0                                                           
         BE    BK20                                                             
*                                                                               
         MVC   P(69),=C'** CASH DISCOUNT IS NOT SUBTRACTED FROM BILLED X        
               AND CLEARED AMOUNTS **'                                          
*                                                                               
BK20     GOTO1 REPORT                                                           
*                                                                               
CLTBKX   DS    0H                                                               
         MVI   CLTACT,C'N'                                                      
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
PPL2WRKD DSECT                                                                  
*                                                                               
*        ACCUMULATORS - 1F FOR EACH MONTH                                       
*        (UP TO 13 MONTHS) PLUS THE TOTAL.                                      
*        INSERT GROSS                          0                                
*        BILL GROSS                            8                                
*        CASH DISCOUNT                        16                                
*        GROSS LESS CD                        24                                
*        PAID                                 32                                
*        B4                                   40                                
*        B5                                   48                                
*        B6                                   56                                
*        B7                                   64                                
*        BILLABLE                             72                                
*        4 BYTES - ONE FOR EACH BILLING TYPE  80-83                             
*        X'01' MEANS BILL FOR THIS TYPE FOUND                                   
*                                                                               
*        LAST 4 BYTES - NOT USED              84-87                             
*                                                                               
         DS    0D                                                               
ACCUM    DS    264PL8            23 MTHS + TOTAL  (24 SETS OF 11PL8)            
*                                                                               
*                                IF X'01' BILLING DONE FOR THAT TYPE            
*                                WILL DISPLAY AS .00 INSTEAD OF BLANKS          
*                                                                               
TOTDSP   EQU   2024              23X88                                          
*                                                                               
ESTGST   DS    CL88              SAME FORMAT AS ACCUM                           
*                                                                               
*        ONLY CLEARED,B4,B5,B6,B7,+BYTES ARE USED                               
*        CLEARED = +32                                                          
*        B4      = +40                                                          
*        B5      = +48                                                          
*        B6      = +56                                                          
*        B7      = +64                                                          
*                  +80(4) X'01' ON FOR EACH BILLING TYPE USED                   
*                  +84(4) NOT USED                                              
*                                                                               
ESTPST   DS    CL88              SAME FORMAT AS ACCUM                           
*                                                                               
*        ONLY CLEARED,B4,B5,B6,B7,+BYTES ARE USED                               
*        CLEARED = +32                                                          
*        B4      = +40                                                          
*        B5      = +48                                                          
*        B6      = +56                                                          
*        B7      = +64                                                          
*                  +80(4) X'01' ON FOR EACH BILLING TYPE USED                   
*                  +84(4) NOT USED                                              
*                                                                               
*                                                                               
PRDTOTS  DS    10PL8               PRODUCT TOTALS                               
PRDGST   DS    10PL8               PRODUCT TOTALS - GST                         
PRDPST   DS    10PL8               PRODUCT TOTALS - PST                         
*                                                                               
CLTTOTS  DS    10PL8               CLIENT TOTALS                                
CLTGST   DS    10PL8               CLIENT TOTALS - GST                          
CLTPST   DS    10PL8               CLIENT TOTALS - PST                          
*                                                                               
WKEFFCD  DS    PL8                 WORKING STORAGE "EFFECTIVE" CD               
*                                                                               
TYPTAB   DS    CL4                 SET TO 4,5,6,7                               
*                                                                               
OLDSW    DS    CL1                 WHERE TO INCLUDE OLD STYLE BILLING           
*                                  SET FROM PROGPROF+1  0, 4-7                  
SAVMAX   DS    CL1                                                              
*                                                                               
CDSW     DS    CL1                 SET FROM PROFILE Y=0 (SUBTRACT CD)           
NETSW    DS    CL1                 SET FROM PROFILE N=O,Y=1(SHOW NET)           
ELCODE   DS    CL1                                                              
*                                                                               
CEU1     DS    CL20                                                             
CEU2     DS    CL20                                                             
*                                                                               
*                                                                               
       ++INCLUDE PPBVALD           NEW PPBVAL DSECT                             
*                                  LIST OF MONTHS FOR ESTIMATE (YM)             
*                                  EOL = X'00'                                  
         DS    0F                                                               
ESTDATA  DS    0CL124                                                           
DTELST   DS    CL48                                                             
*                                                                               
BILLDT   DS    CL2                                                              
INSDTE   DS    CL2                                                              
SW       DS    CL1                                                              
*                                  N=1 (DON'T SUBTRACT CD)                      
FRSTIM   DS    CL1                                                              
DATE     DS    CL6                                                              
SVKEYS   DS    CL64                                                             
*                                                                               
*                                  END OF ESTDATA                               
*                                                                               
STHD     DS    CL22                                                             
TDHD     DS    CL20                                                             
LOWDTE   DS    XL3                                                              
HIDTE    DS    XL3                                                              
PRDACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
WKX      EQU   *                                                                
*                                                                               
GVALUESD DSECT                                                                  
       ++INCLUDE GVALUES                                                        
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE         HAVE NEW PBILLREC DESCT                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030PPREPL202 04/01/13'                                      
         END                                                                    
