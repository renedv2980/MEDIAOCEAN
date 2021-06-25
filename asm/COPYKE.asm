*          DATA SET COPYKE     AT LEVEL 015 AS OF 05/01/02                      
*PHASE COPYKE,*,NOAUTO                                                          
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE GETINS                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'COPYKE- COPY KETCHEM DATA TO WESTERN'                           
*                                                                               
*              COPIES ONLY SELECTED CLIENT DATA                                 
*                                                                               
COPYKE   CSECT                                                                  
         NBASE 0,COPYKE,=V(REGSAVE)                                             
         SPACE 2                                                                
         LA    R6,COPYKE+4095                                                   
         LA    R6,1(R6)                                                         
         USING COPYKE+4096,R6                                                   
*                                  NOTE USE OF R6 AS BASE REGISTER              
         USING ETABD,R8                                                         
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*&&DO                                                                           
         OPEN  IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*&&                                                                             
*                                                                               
*                                  SET LOOKUP TABLE BINSRCH PARS                
         SR    R0,R0                                                            
         L     R1,=A(LKTAB)                                                     
         SR    R2,R2                                                            
         LA    R3,11                                                            
         LA    R4,11                                                            
         LA    R5,200                                                           
         STM   R0,R5,LKPARS                                                     
*                                  SET ESTTAB BINSRCH PARS                      
         SR    R0,R0                                                            
         L     R1,=A(ESTTAB)                                                    
         SR    R2,R2                                                            
         LA    R3,ETABEL                                                        
         LA    R4,11                                                            
         L     R5,=F'6000'                                                      
         STM   R0,R5,ESTPARS                                                    
*                                                                               
START1   DS    0H                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
START2   DS    0H                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3                                                           
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
START3   DS    0H                                                               
         XC    WORK(11),WORK                                                    
         MVC   WORK(6),CARD+2                                                   
         CLC   CARD+5(3),=C'ALL'                                                
         BNE   *+10                                                             
         MVC   WORK+3(3),=3X'FF'                                                
         MVC   WORK+6(3),CARD+11                                                
         CLC   CARD+11(3),=C'ALL'                                               
         BNE   *+10                                                             
         MVC   WORK+6(3),=3X'FF'                                                
         MVC   WORK+9(2),=3X'FF'                                                
         CLC   CARD+20(3),=C'ALL'                                               
         BE    START3B                                                          
         CLI   CARD+20,C' '                                                     
         BE    START3B                                                          
         PACK  DUB,CARD+20(3)                                                   
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   WORK+9(2),HALF                                                   
START3B  DS    0H                                                               
         MVC   P+1(80),CARD                                                     
         BAS   RE,PRNT                                                          
         GOTO1 =V(BINSRCH),LKPARS,(1,WORK)                                      
*                                                                               
         OC    LKPARS+1(3),LKPARS+1                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     START1                                                           
START10  DS    0H                                                               
         XC    X,X                                                              
         OC    LKPARS+8(4),LKPARS+8                                             
         BNZ   START12                                                          
         MVC   P(18),=C'**NO INPUT CARDS**'                                     
         BAS   RE,PRNT                                                          
         B     EOJ                                                              
START12  DS    0H                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
*              ADD CODE FOR THIS RUN HERE                                       
*                                                                               
         CLI   REC,C'Z'         SPECIAL DDS RECORD                              
         BE    EOF              PAST ALL REAL DATA                              
         CLC   REC(3),=C'KAO'      SEE IF PAST KETCHUM OUTDOOR                  
         BH    EOF                 DONE                                         
         CLC   REC(3),=C'KAM'      OR BEFORE KETCHUM MAGAZINES                  
         BL    GET                 DONE                                         
         CLI   REC+3,X'02'         CLIENT                                       
         BE    CLT                                                              
         CLI   REC+3,X'03'         DIVISION (SAME AS CLIENT)                    
         BE    CLT                                                              
         CLI   REC+3,X'04'         REGION (SAME AS CLIENT)                      
         BE    CLT                                                              
         CLI   REC+3,X'05'         DISTRICT (SAME AS CLT)                       
         BE    CLT                                                              
         CLI   REC+3,X'06'         PRODUCT                                      
         BE    PRD                                                              
         CLI   REC+3,X'07'         ESTIMATES                                    
         BE    EST                                                              
         CLI   REC+3,X'09'         BUCKETS (SAME AS EST)                        
         BE    EST                                                              
         CLI   REC+3,X'15'         JOB RECORDS                                  
         BE    PRD                 JOBS SAME AS PRD                             
         CLI   REC+3,X'20'         BUY                                          
         BE    BUY                                                              
         B     GET                 IGNORE OTHER RECORDS                         
*                                                                               
         SPACE 3                                                                
CLT      DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         MVC   WORK+6(3),=C'   '                                                
         B     CHECKCP                                                          
*                                                                               
PRD      DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         MVC   WORK+6(3),REC+7                                                  
         B     CHECKCP                                                          
*                                                                               
EST      DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         MVC   WORK+6(3),REC+7                                                  
         MVC   WORK+9(2),REC+10                                                 
         B     CHECK                                                            
*                                                                               
CHECKCP  DS    0H                                                               
         LA    R8,CPTAB                                                         
CHECKCP5 CLI   0(R8),X'FF'       END OF CLT/PRD TAB                             
         BE    GET                                                              
         CLC   0(9,R8),WORK                                                     
         BE    CHECK5                                                           
         LA    R8,9(R8)                                                         
         B     CHECKCP5                                                         
*                                                                               
CHECK    BAS   R9,SRCH                                                          
         BNE   GET                                                              
*                                                                               
CHECK5   DS    0H                                                               
         CLI   REC+3,X'15'       SEE IF DOING JOB                               
         BNE   CHK20                                                            
         LA    R8,JOBTAB                                                        
CHK5     CLI   0(R8),X'FF'      END OF TABLE                                    
         BE    GET                                                              
         CLC   WORK+2(7),0(R8)   MATCH MED/CLT/PRD                              
         BE    CHK6                                                             
CHK5X    LA    R8,13(R8)                                                        
         B     CHK5                                                             
*                                                                               
CHK6     CLC   REC+4(12),1(R8)   CHECK JOB                                      
         BE    CHK7                                                             
         B     CHK5X                                                            
*                                                                               
CHK7     DS    0H                                                               
         OC    REC+16(6),REC+16                                                 
         BZ    CHK70                                                            
         CLC   REC+16(6),=X'FFFFFFFFFFFF'                                       
         BE    CHK70                                                            
         B     GET                                                              
*                                                                               
CHK20    DS    0H                                                               
         CLI   REC+3,X'20'         SEE IF BUY REC                               
         BNE   CHK70                                                            
         LA    R8,REC+10                                                        
         BAS   RE,CONPUB           GO CONVERT PUB                               
         B     CHK70                                                            
*                                                                               
CHK70    DS    0H                                                               
         MVC   WORK(2),=C'WI'     CHANGE TO WESTERN                             
*                                 FOR ESTTAB                                    
*                                                                               
         CLI   REC+3,X'20'                                                      
         BNE   CHK75                                                            
         GOTO1 =V(BINSRCH),ESTPARS,(1,WORK)                                     
*                                                                               
         L     R8,ESTPARS                                                       
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHK75    DS    0H                                                               
         LA    RF,CLTCNT                                                        
         CLI   REC+3,X'02'        CLIENTS                                       
         BE    CLT10                                                            
         LA    RF,DIVCNT                                                        
         CLI   REC+3,X'03'        DIVISIONS                                     
         BE    CLT10                                                            
         LA    RF,REGCNT                                                        
         CLI   REC+3,X'04'        REGIONS                                       
         BE    CLT10                                                            
         LA    RF,DSTCNT                                                        
         CLI   REC+3,X'05'        DISTRICTS                                     
         BE    CLT10                                                            
         LA    RF,PRDCNT                                                        
         CLI   REC+3,X'06'        PRODUCTS                                      
         BE    CLT10                                                            
         LA    RF,JOBCNT                                                        
         CLI   REC+3,X'15'        JOBRECS                                       
         BE    CLT10                                                            
         LA    RF,ESTCNT                                                        
         CLI   REC+3,X'07'        ESTIMATES                                     
         BE    CLT10                                                            
         LA    RF,BKTCNT                                                        
         CLI   REC+3,X'09'        ESTIMATE BUCKETS                              
         BE    CLT10                                                            
         DC    H'0'               UNKNOWN RECORD TYPE                           
CLT10    AP    0(5,RF),=P'1'                                                    
*                                                                               
         MVC   REC(2),=C'WI'       CHANGE RECORD TO WESTERN                     
         CLI   REC+3,X'20'         MUST BE BUY REC                              
         BNE   *+8                 TO SET ETESTSW                               
         MVI   ETESTSW,C'E'                                                     
         CLI   PRTSW,C'Y'                                                       
         BNE   PUT                                                              
         MVI   DMPSW,C'Y'                                                       
         B     PUT                                                              
         SPACE 3                                                                
BUY      DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),REC                                                      
         MVC   WORK+3(3),REC+4                                                  
         MVC   WORK+6(3),REC+7                                                  
         MVC   WORK+9(2),REC+19        ESTIMATE                                 
         BAS   R9,SRCH                                                          
         BNE   GET                                                              
*                                                                               
         LA    R8,REC+10                                                        
         BAS   RE,CONPUB           GO CONVERT PUB                               
*                                                                               
         MVC   REC(2),=C'WI'          CHANGE TO WESTERN                         
         MVC   WORK(2),=C'WI'         CHANGE FOR ESTTAB                         
*                                                                               
         GOTO1 =V(BINSRCH),ESTPARS,(1,WORK)                                     
         L     R8,ESTPARS                                                       
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(GETINS),DMCB,REC,GROSS,REC+7                                  
*                                                                               
         TM    REC+27,X'20'                                                     
         BZ    *+10                                                             
         XC    GROSS(20),GROSS                                                  
         L     RF,ETBUYS                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ETBUYS                                                        
*                                                                               
         LM    R0,R2,ETBUYGRS                                                   
         A     R0,GROSS                                                         
         A     R1,AGYCOM                                                        
         A     R2,CSHDSC                                                        
         STM   R0,R2,ETBUYGRS                                                   
*                                                                               
         LM    R0,R2,ETPAYGRS                                                   
         A     R0,PGROSS                                                        
         A     R1,PAGYCOM                                                       
         A     R2,PCSHDSC                                                       
         STM   R0,R2,ETPAYGRS                                                   
*                                                                               
         CLI   PRTSW,C'Y'                                                       
         BNE   PUT                                                              
         MVI   DMPSW,C'Y'                                                       
         B     PUT                                                              
         SPACE 3                                                                
SRCH     DS    0H                                                               
         MVC   X(11),WORK                                                       
SRCH2    DS    0H                                                               
         GOTO1 =V(BINSRCH),LKPARS,X                                             
*                                                                               
         CLI   LKPARS,0                                                         
         BER   R9                                                               
         CLI   X+9,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+9(2),=2X'FF'                                                   
         B     SRCH2                                                            
         CLI   X+6,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+6(3),=3X'FF'                                                   
         B     SRCH2                                                            
         CLI   X+3,X'FF'                                                        
         BE    *+14                                                             
         MVC   X+3(3),=3X'FF'                                                   
         B     SRCH2                                                            
*                                                                               
SRCH6    DS    0H                                                               
         CLI   LKPARS,0                                                         
         BR    R9                                                               
*                                                                               
*                                                                               
PUT      DS    0H                                                               
         MVI   BYTE,X'80'                                                       
         TM    REC+27,X'20'        TEST DELETED                                 
         BNZ   *+8                                                              
         MVI   BYTE,0                                                           
         MVC   REC+27(1),BYTE                                                   
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
         SPACE 2                                                                
*&&DO                                                                           
EOF      CLOSE IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
*&&                                                                             
         BAS   RE,PRNT                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                  PRINT ESTIMATE LIST                          
         BAS   RE,SKIP                                                          
         MVC   P+31(18),=C'PRINTPAK DATA COPY'                                  
         MVC   P+71(8),TODAY                                                    
         BAS   RE,PRNT3                                                         
         MVC   P+01(78),=C'                                           GX        
               ROSS       GROSS-CD       GROSS-CD'                              
         BAS   RE,PRNT                                                          
         MVC   P+01(78),=C'AG M CLT PRD EST     BUYS   BILLS        ORDX        
               ERED           PAID         BILLED'                              
         BAS   RE,PRNT                                                          
         MVC   P+01(78),=C'-- - --- --- ---     ----   -----        ---X        
               ----           ----         ------'                              
         BAS   RE,PRNT2                                                         
         L     R8,ESTPARS+4                                                     
         L     R2,ESTPARS+8                                                     
         LTR   R2,R2                                                            
         BZ    EOF10                                                            
         LA    R1,RTOTS            RUN TOTALS                                   
         LA    R0,11                                                            
EOF2C    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,EOF2C                                                         
         LA    R1,MTOTS            AGY/MEDIA TOTALS                             
         LA    R0,11                                                            
EOF2E    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,EOF2E                                                         
         XC    SAVAMED,SAVAMED                                                  
*                                                                               
EOF3     DS    0H                                                               
         CLI   SAVAMED,0           FIRST MEDIA                                  
         BNE   EOF3C                                                            
         MVC   SAVAMED,ETAGY                                                    
EOF3C    CLC   SAVAMED,ETAGY       CHK FOR CHANGE OF AGY MEDIA                  
         BE    EOF3G                                                            
         ST    R8,FULL                                                          
         LA    R8,MTOTS                                                         
         MVC   P+1(11),=C'*A/M TOTAL*'                                          
         BAS   R9,EDTPK            PACKED EDIT                                  
         BAS   RE,PRNT                                                          
         MVC   SAVAMED,ETAGY                                                    
         LA    R1,MTOTS                                                         
         LA    R0,11                                                            
EOF3E    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,EOF3E                                                         
         L     R8,FULL             RESTORE R8                                   
*                                                                               
EOF3G    DS    0H                                                               
         LA    R3,ETBUYS                                                        
         LA    R0,11                                                            
         LA    R7,MTOTS                                                         
EOF3K    DS    0H                                                               
         L     RF,0(R3)                                                         
         CVD   RF,DUB                                                           
         AP    0(8,R7),DUB                                                      
         LA    R3,4(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R0,EOF3K                                                         
*                                                                               
         MVC   P+1(2),ETAGY                                                     
         MVC   P+4(1),ETMED                                                     
         MVC   P+6(3),ETCLT                                                     
         MVC   P+10(3),ETPRD                                                    
         EDIT  (B2,ETEST),(3,P+14)                                              
         BAS   R9,EDT                                                           
         LA    R3,ETBUYS                                                        
         LA    R0,11                                                            
         LA    R7,RTOTS                                                         
EOF4     DS    0H                                                               
         L     RF,0(R3)                                                         
         CVD   RF,DUB                                                           
         AP    0(8,R7),DUB                                                      
         LA    R3,4(R3)                                                         
         LA    R7,8(R7)                                                         
         BCT   R0,EOF4                                                          
*                                                                               
         BAS   RE,PRNT                                                          
         LA    R8,ETABEL(R8)                                                    
         BCT   R2,EOF3                                                          
*                                                                               
         LA    R8,MTOTS            DO LAST AGY/MEDIA                            
         MVC   P+1(11),=C'*A/M TOTAL*'                                          
         BAS   R9,EDTPK                                                         
         BAS   RE,PRNT                                                          
*                                                                               
         MVC   P+1(11),=C'*RUN TOTAL*'                                          
         LA    R8,RTOTS                                                         
         BAS   R9,EDTPK                                                         
         BAS   RE,PRNT                                                          
EOF10    DS    0H                                                               
         B     EOJ                                                              
*                                                                               
*  OLD EDT                                                                      
*DT      DS    0H                                                               
         L     R0,ETBUYGRS                                                      
         EDIT  (R0),(14,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYGRS                                                      
         S     R0,ETPAYCD                                                       
         EDIT  (R0),(14,P+51),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILGRS                                                      
         S     R0,ETBILCD                                                       
         EDIT  (R0),(14,P+66),2,COMMAS=YES,MINUS=YES                            
         EDIT  (B4,ETBUYS),(7,P+19),COMMAS=YES                                  
         EDIT  (B4,ETBILLS),(7,P+27),COMMAS=YES                                 
         BR    R9                                                               
*                                                                               
EDT      DS    0H                                                               
         MVI   P+35,C'G'                                                        
         EDIT  (B4,ETBUYS),(7,P+19),COMMAS=YES                                  
         EDIT  (B4,ETBILLS),(7,P+27),COMMAS=YES                                 
         L     R0,ETBUYGRS                                                      
         EDIT  (R0),(14,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYGRS                                                      
         EDIT  (R0),(14,P+51),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILGRS                                                      
         EDIT  (R0),(14,P+66),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         MVI   P+35,C'A'                                                        
         L     R0,ETBUYAC                                                       
         EDIT  (R0),(14,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYAC                                                       
         EDIT  (R0),(14,P+51),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILAC                                                       
         EDIT  (R0),(14,P+66),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         MVI   P+35,C'C'                                                        
         L     R0,ETBUYCD                                                       
         EDIT  (R0),(14,P+36),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETPAYCD                                                       
         EDIT  (R0),(14,P+51),2,COMMAS=YES,MINUS=YES                            
         L     R0,ETBILCD                                                       
         EDIT  (R0),(14,P+66),2,COMMAS=YES,MINUS=YES                            
         BAS   RE,PRNT                                                          
         BR    R9                                                               
*OLD EDTPK                                                                      
*DTPK    DS    0H                                                               
         ZAP   MYDUB,16(8,R8)      ORD GRS                                      
         EDIT  MYDUB,(14,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,40(8,R8)      PAY GROSS                                    
         SP    MYDUB,56(8,R8)      PAY CD                                       
         EDIT  MYDUB,(14,P+51),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,64(8,R8)      BILL GROSS                                   
         SP    MYDUB,80(8,R8)      BILL CD                                      
         EDIT  MYDUB,(14,P+66),2,COMMAS=YES,MINUS=YES                           
         EDIT  (P8,0(R8)),(7,P+19),COMMAS=YES                                   
         EDIT  (P8,8(R8)),(7,P+27),COMMAS=YES                                   
         BR    R9                                                               
********************************************************                        
*                                                                               
EDTPK    DS    0H                                                               
         EDIT  (P8,0(R8)),(7,P+19),COMMAS=YES                                   
         EDIT  (P8,8(R8)),(7,P+27),COMMAS=YES                                   
         MVI   P+35,C'G'                                                        
         ZAP   MYDUB,16(8,R8)      ORD GRS                                      
         EDIT  MYDUB,(14,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,40(8,R8)      PAY GROSS                                    
         EDIT  MYDUB,(14,P+51),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,64(8,R8)      BILL GROSS                                   
         EDIT  MYDUB,(14,P+66),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         MVI   P+35,C'A'                                                        
         ZAP   MYDUB,24(8,R8)      ORD AC                                       
         EDIT  MYDUB,(14,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,48(8,R8)      PAY AC                                       
         EDIT  MYDUB,(14,P+51),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,72(8,R8)      BILL AC                                      
         EDIT  MYDUB,(14,P+66),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         MVI   P+35,C'C'                                                        
         ZAP   MYDUB,32(8,R8)      ORD CD                                       
         EDIT  MYDUB,(14,P+36),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,56(8,R8)      PAY CD                                       
         EDIT  MYDUB,(14,P+51),2,COMMAS=YES,MINUS=YES                           
         ZAP   MYDUB,80(8,R8)      BILL CD                                      
         EDIT  MYDUB,(14,P+66),2,COMMAS=YES,MINUS=YES                           
         BAS   RE,PRNT                                                          
         BR    R9                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         SPACE 2                                                                
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
         SPACE 3                                                                
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
CONPUB   NTR1                                                                   
         LA    R1,MPUBTAB      MAGAZINE TABLE                                   
         CLI   REC+2,C'M'                                                       
         BE    CONPUB5                                                          
         LA    R1,NPUBTAB      NEWSPAPER TABLE                                  
         CLI   REC+2,C'N'                                                       
         BE    CONPUB5                                                          
         LA    R1,OPUBTAB      OUTDOOR TABLE                                    
         CLI   REC+2,C'O'                                                       
         BE    CONPUB5                                                          
         DC    H'0'             BAD MEDIA CODE                                  
*                                                                               
CONPUB5  CLC   0(2,R1),=X'FFFF'     END OF TABLE                                
         BNE   CONPUB7                                                          
         MVC   P(16),=C'PUB NOT IN TABLE'                                       
         BAS   RE,PRNT                                                          
         BAS   RE,DMPKEY                                                        
         B     CONPUBX           DUMP FOR NOW                                   
*                                                                               
CONPUB7  CLC   0(6,R1),REC+10                                                   
         BE    CONPUB10                                                         
         LA    R1,12(R1)                                                        
         B     CONPUB5                                                          
*                                                                               
CONPUB10 MVC   REC+10(6),6(R1)    SWITCH PUB CODES                              
CONPUBX  XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
DMPREC   NTR1                                                                   
*                                                                               
         LA    R5,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R2,4(R2)                                                         
         LA    R3,0(R5,R2)         EOR                                          
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   XIT                                                              
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N'                             
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,PRNT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         B     XIT                                                              
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         LA    R2,KLEN                                                          
         GOTO1 =V(HEXOUT),DMCB,(R5),P+01,(R2),=C'N'                             
*                                                                               
         MVC   DWORK(KLEN),0(R5)                                                
         TR    DWORK(KLEN),TRTAB                                                
         MVC   P+75(KLEN),DWORK                                                 
         B     XIT                                                              
         SPACE 3                                                                
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
*                                                                               
         CLC   REC+25(2),=X'80FF'     DIRECTORY ONLY                            
         BE    GETR5                                                            
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
*                                                                               
GETR5    AP    INCNT,=P'1'                                                      
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
PUTREC   NTR1                                                                   
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
         BAS   RE,DMPREC                                                        
PUTREC2  DS    0H                                                               
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
         B     XIT                                                              
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
DWORK    DS    CL100                                                            
*                                                                               
*&&DO                                                                           
IN       DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=INPUT,             X        
               IOAREA1=IN1,DEVADDR=SYS010,FILABL=NO,WORKA=YES,         X        
               EOFADDR=EOF                                                      
*&&                                                                             
*&&OS                                                                           
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*&&                                                                             
         EJECT                                                                  
*&&DO                                                                           
OUT      DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=OUTPUT,            X        
               IOAREA1=OUT1,DEVADDR=SYS011,FILABL=NO,WORKA=YES                  
*&&                                                                             
*&&OS                                                                           
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*&&                                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CPTAB    DC    CL9'KAMAHMCL '                                                   
         DC    CL9'KAMAHMFUL'                                                   
         DC    CL9'KAMAHMNSX'                                                   
         DC    CL9'KAMAHMRL '                                                   
         DC    CL9'KAMAHM   '                                                   
*                                                                               
         DC    CL9'KANAHMCL '                                                   
         DC    CL9'KANAHMFUL'                                                   
         DC    CL9'KANAHMNSX'                                                   
         DC    CL9'KANAHM   '                                                   
         DC    CL9'KANAHNADV'                                                   
         DC    CL9'KANAHN   '                                                   
         DC    CL9'KANAHTADV'                                                   
         DC    CL9'KANAHTRL '                                                   
         DC    CL9'KANAHTCL '                                                   
         DC    CL9'KANAHT   '                                                   
*                                                                               
         DC    CL9'KAOAHMNSX'                                                   
         DC    CL9'KAOAHM   '                                                   
         DC    X'FF'                                                            
*                                                                               
*        TABLE OF JOBS TO BE COPIED                                             
*                                  MAGAZINE JOBS                                
JOBTAB   DC    CL13'MAHMCL 37340A'                                              
         DC    CL13'MAHMCL 37340B'                                              
         DC    CL13'MAHMCL 50124Z'                                              
         DC    CL13'MAHMCL 50311B'                                              
         DC    CL13'MAHMCL 50311C'                                              
         DC    CL13'MAHMCL 50311D'                                              
         DC    CL13'MAHMFUL50124B'                                              
         DC    CL13'MAHMFUL50124Z'                                              
         DC    CL13'MAHMFUL50101B'                                              
         DC    CL13'MAHMNSX50296A'                                              
         DC    CL13'MAHMNSX50296B'                                              
         DC    CL13'MAHMNSX50296C'                                              
         DC    CL13'MAHMNSX50296D'                                              
         DC    CL13'MAHMNSX50296E'                                              
         DC    CL13'MAHMNSX50296G'                                              
         DC    CL13'MAHMRL 50310B'                                              
         DC    CL13'MAHMRL 50310C'                                              
*                                  NEWSPAPER                                    
         DC    CL13'NAHMCL 50125D'                                              
         DC    CL13'NAHMCL 50125D'                                              
         DC    CL13'NAHMCL 50125F'                                              
         DC    CL13'NAHMCL 50125E'                                              
         DC    CL13'NAHMCL 50206 '                                              
         DC    CL13'NAHMFUL50318Y'                                              
         DC    CL13'NAHMFUL50318Z'                                              
         DC    CL13'NAHMNSX50297 '                                              
         DC    CL13'NAHNRL ZONE6 '                                              
         DC    CL13'NAHTADV50358E'                                              
         DC    CL13'NAHTADV50367E'                                              
         DC    X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
MPUBTAB  DC    X'864129080000',X'000060450000'                                  
         DC    X'803020040000',X'000050250000'                                  
         DC    X'000103490000',X'000063430000'                                  
         DC    X'830080000000',X'000058490000'                                  
         DC    X'000108510000',X'000063440000'                                  
         DC    X'876120040000',X'000053060000'                                  
         DC    X'873020010000',X'000058570000'                                  
         DC    X'000107290000',X'000063450000'                                  
         DC    X'000101440000',X'000063460000'                                  
         DC    X'000106160000',X'000054530000'                                  
         DC    X'000107270000',X'000063470000'                                  
         DC    X'000102410000',X'000063480000'                                  
         DC    X'000104110000',X'000063490000'                                  
         DC    X'000101630000',X'000063500000'                                  
         DC    X'000101350000',X'000061180000'                                  
         DC    X'000108190000',X'000062120000'                                  
         DC    X'000108160000',X'000063510000'                                  
         DC    X'000108170000',X'000058490100'                                  
         DC    X'000143080000',X'000003270100'                                  
         DC    X'000108090000',X'000063520000'                                  
         DC    X'000107560000',X'000063530000'                                  
         DC    X'000101660000',X'000063540000'                                  
         DC    X'865018100000',X'000063550000'                                  
         DC    X'865240050000',X'000052100000'                                  
         DC    X'803360020000',X'000050010000'                                  
         DC    X'889038990000',X'000058000000'                                  
         DC    X'FFFF'             END OF MAGAZINE TABLE                        
*                                                                               
NPUBTAB  DC    X'111113070100',X'000005780000'                                  
         DC    X'112173510000',X'000008370000'                                  
         DC    X'111433550000',X'000006460000'                                  
         DC    X'113620050000',X'000012030000'                                  
         DC    X'113640010000',X'000012050000'                                  
         DC    X'114550090000',X'000013500000'                                  
         DC    X'112302010000',X'000008560000'                                  
         DC    X'111011500000',X'000005470000'                                  
         DC    X'114506990000',X'000013710000'                                  
         DC    X'112000830000',X'000009330000'                                  
         DC    X'110512570000',X'000003010000'                                  
         DC    X'110556010000',X'000003460000'                                  
         DC    X'113921040000',X'000012800000'                                  
         DC    X'113957000000',X'000012880000'                                  
         DC    X'113824010000',X'000012440000'                                  
         DC    X'110624030000',X'000004950000'                                  
         DC    X'112690060000',X'000009460000'                                  
         DC    X'110683510000',X'000004820000'                                  
         DC    X'111040090000',X'000005760000'                                  
         DC    X'112325030000',X'000009020000'                                  
         DC    X'112487550000',X'000015380000'                                  
         DC    X'105037540000',X'000014590000'                                  
         DC    X'105000070000',X'000016000000'                                  
         DC    X'113300020000',X'000003270000'                                  
         DC    X'110575000000',X'000003920000'                                  
         DC    X'FFFF'            END OF NEWSPAPER TABLE                        
*                                                                               
OPUBTAB  DC    X'000580330000',X'000580330000'                                  
         DC    X'000053500700',X'000053500700'                                  
         DC    X'000580030000',X'000580030000'                                  
         DC    X'000582740000',X'000582740000'                                  
         DC    X'000582730000',X'000582730000'                                  
         DC    X'000582750000',X'000582750000'                                  
         DC    X'000582760000',X'000582760000'                                  
         DC    X'000582440000',X'000582440000'                                  
         DC    X'000582720000',X'000582720000'                                  
         DC    X'000579160000',X'000579160000'                                  
         DC    X'000579170000',X'000579170000'                                  
         DC    X'FFFF'                                                          
                                                                                
         SPACE 2                                                                
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
RTOTS    DS    11PL8               RUN TOTALS                                   
MTOTS    DS    11PL8               AGY MEDIA TOTALS                             
SAVAMED  DS    CL3                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
CLTCNT   DC    PL5'0',CL20'CLIENTS'                                             
DIVCNT   DC    PL5'0',CL20'DIVISIONS'                                           
REGCNT   DC    PL5'0',CL20'REGIONS'                                             
DSTCNT   DC    PL5'0',CL20'DISTRICTS'                                           
PRDCNT   DC    PL5'0',CL20'PRODUCTS'                                            
ESTCNT   DC    PL5'0',CL20'ESTIMATES'                                           
BKTCNT   DC    PL5'0',CL20'EST BUCKETS'                                         
JOBCNT   DC    PL5'0',CL20'JOB RECORDS'                                         
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    3000C                                                            
         DS    D                                                                
*&&DO                                                                           
IN1      DS    8500C                                                            
*&&                                                                             
*&&DO                                                                           
OUT1     DS    8500C                                                            
*&&                                                                             
         SPACE 3                                                                
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PBILLREC                                                       
*                                                                               
ETABD    DSECT                                                                  
ETAGY    DS    CL2                                                              
ETMED    DS    CL1                                                              
ETCLT    DS    CL3                                                              
ETPRD    DS    CL3                                                              
ETEST    DS    XL2                                                              
ETESTSW  DS    CL1                                                              
ETBUYS   DS    F                                                                
ETBILLS  DS    F                                                                
ETBUYGRS DS    F                                                                
ETBUYAC  DS    F                                                                
ETBUYCD  DS    F                                                                
ETPAYGRS DS    F                                                                
ETPAYAC  DS    F                                                                
ETPAYCD  DS    F                                                                
ETBILGRS DS    F                                                                
ETBILAC  DS    F                                                                
ETBILCD  DS    F                                                                
ETABEL   EQU   *-ETABD                                                          
*                                                                               
ESTTAB   CSECT                                                                  
         DS    6000CL56            ROOM FOR 6000 ESTS                           
         DC    X'0000'                                                          
*                                                                               
LKTAB    CSECT                                                                  
         DS    200CL11                                                          
         DC    X'0000'                                                          
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015COPYKE    05/01/02'                                      
         END                                                                    
