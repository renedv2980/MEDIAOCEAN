*          DATA SET ACREP7102  AT LEVEL 072 AS OF 07/23/13                      
*PHASE AC7102C                                                                  
*INCLUDE SQUASHER                                                               
         TITLE 'PRODUCTION PROFILE LISTING'                                     
AC7102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC7102                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GENERAL WORK)                           
         LA    R9,SPACEND                                                       
         USING WRKD,R9             R9=A(TEMP W/S)                               
         MVC   RECTYPE,SPACES                                                   
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   ACP2                                                             
         L     R3,=A(BLOCK)                                                     
         ST    R3,ABLOCK           SAVE ADDRESS OF PRINT BUFFER                 
         L     R3,=V(SQUASHER)                                                  
         ST    R3,SQUASHER                                                      
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   EXIT                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
*              GO TO APPROPRIATE ROUTINES'                                      
*                                                                               
ACP2     CLI   MODE,PROCLEVA                                                    
         BNE   ACP4                                                             
         ZAP   ESTTOT,=P'0'                                                     
         L     R2,ADHEIRA                                                       
         MVC   RECTYPE,=C'CLIENT '                                              
         MVI   FORCEHED,C'Y'                                                    
         B     ACP8                                                             
         SPACE 1                                                                
ACP4     CLI   MODE,PROCLEVB                                                    
         BNE   ACP5                                                             
         ZAP   ESTTOT,=P'0'                                                     
         L     R2,ADHEIRB                                                       
         MVC   RECTYPE,=C'PRODUCT'                                              
         B     ACP8                                                             
         SPACE 1                                                                
ACP5     CLI   MODE,PROCLEVC                                                    
         BNE   ACP7                                                             
         CLI   QOPT2,C'Y'          OPTION TO PRINT CLI/PRO ONLY                 
         BE    EXIT                                                             
         L     R2,ADACC                                                         
         MVC   RECTYPE,=C'JOB    '                                              
         L     R3,ADACCSTA         SEE IF SUPPRESSION REQUIRED                  
         USING ACSTATD,R3                                                       
         CLI   QOPT1,C'S'                                                       
         BNE   ACP6                                                             
         TM    ACSTSTAT,X'40'                                                   
         BO    EXIT                                                             
         SPACE 1                                                                
ACP6     BAS   RE,LOOKUP                                                        
         B     ACP8                                                             
         DROP  R3                                                               
         SPACE 1                                                                
ACP7     CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*              INITIALIZE PRINT BUFFER & HANDLE NAME ELEMENTS                   
*                                                                               
ACP8     L     R3,ABLOCK           INITIALIZE PRINT BLOCK                       
         LH    R4,BLOCKEN                                                       
         MVC   0(110,R3),SPACES                                                 
         LA    R3,110(R3)                                                       
         BCT   R4,*-10                                                          
         MVC   PWORK,SPACES                                                     
         MVC   PWORK(12),3(R2)     ACCOUNT CODE                                 
         AH    R2,DATADISP                                                      
         LR    R3,R2                                                            
         SR    R4,R4                                                            
         SPACE 1                                                                
ACP10    CLI   0(R3),0             FIND NAME ELEMENT IN RECORD                  
         BE    EXIT                                                             
         CLI   0(R3),X'20'                                                      
         BE    ACP12                                                            
         IC    R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     ACP10                                                            
         SPACE 1                                                                
ACP12    IC    R4,1(R3)                                                         
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   PWORK+13(0),2(R3)   MOVE NAME TO WORK                            
         GOTO1 SQUASHER,DMCB,PWORK,110                                          
         CLC   RECTYPE,=C'CLIENT '                                              
         BNE   *+10                                                             
         MVC   CLINAME,PWORK       SAVE IF CLIENT NAME                          
         GOTO1 CHOPPER,DMCB,(51,PWORK),(25,PWORK2),(0,2)                        
         L     R4,ABLOCK                                                        
         MVC   0(7,R4),RECTYPE                                                  
         MVC   9(25,R4),PWORK2                                                  
         MVC   119(25,R4),PWORK2+25                                             
         LA    R5,36(R4)           R5=A(START OF OUTPUT BLOCK)                  
         EJECT                                                                  
*              PROCESS ALL ELEMENTS ON THIS RECORD                              
*                                                                               
ACP14    LA    R3,ELTAB            R3=A(PROCESSABLE ELEMENTS TABLE)             
         CLI   0(R2),0             END OF RECORD                                
         BE    ACP22                                                            
         SPACE 1                                                                
ACP16    CLI   0(R3),X'FF'         END OF ELEMENT TABLE                         
         BE    ACP18                                                            
         CLC   0(1,R2),0(R3)       MATCH TABLE ENTRY WITH ELEMENT               
         BE    ACP20                                                            
         LA    R3,L'ELTAB(R3)                                                   
         B     ACP16                                                            
         SPACE 1                                                                
ACP18    SR    R4,R4               BUMP TO NEXT ELEMENT                         
         IC    R4,1(R2)                                                         
         AR    R2,R4                                                            
         B     ACP14                                                            
         SPACE 1                                                                
* GO TO ROUTINE PASSING - R2=A(ELEMENT)                                         
*                         R5=A(NEXT AVAILABLE OUTPUT AREA)                      
*                                                                               
ACP20    L     RF,0(R3)                                                         
         BASR  RE,RF                                                            
         B     ACP18               GET NEXT ELEMENT                             
         SPACE 1                                                                
*              TABLE OF ELEMENTS AND ADDRESSES OF ROUTINES TO                   
*              PROCESS THEM                                                     
         CNOP  0,4                                                              
ELTAB    DS    0CL4                                                             
         DC    X'22',AL3(ACPADD)  *ADDRESS                                      
         DC    X'21',AL3(ACPNUM)  *NUMBER (BILLING)                             
         DC    X'24',AL3(ACPPRA)  *PROFILE (BASIC)                              
         DC    X'3C',AL3(ACPPRB)  *PROFILE (EXTRA)                              
         DC    X'30',AL3(ACPSTA)  *STATUS                                       
         DC    X'3E',AL3(ACPCOM)  *COMMENTS                                     
         DC    X'26',AL3(ACPJOB)  *JOB                                          
         DC    X'3A',AL3(ACPPOL)  *POOL                                         
         DC    X'42',AL3(ACPRUL)  *RULES                                        
         DC    X'3D',AL3(ACPSAL)  *SALES ANALYSIS                               
         DC    X'FFFF'                                                          
         SPACE 1                                                                
*              PRINT A BUFFER                                                   
*                                                                               
ACP22    BAS   RE,ACPBDG2                                                       
         MVC   P,SPACES                                                         
         BAS   RE,ACPRINT                                                       
         L     R5,ABLOCK                                                        
         LH    R4,BLOCKEN                                                       
         SPACE 1                                                                
ACP24    CLC   0(110,R5),SPACES                                                 
         BE    EXIT                                                             
         MVC   P+1(110),0(R5)                                                   
         BAS   RE,ACPRINT                                                       
         LA    R5,110(R5)                                                       
         BCT   R4,ACP24                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              FORMAT ADDRESS ELEMENT                                           
*                                                                               
ACPADD   NTR1                                                                   
         USING ACADDD,R2                                                        
         MVC   0(16,R5),=C'BILLING ADDRESS='                                    
         SR    R4,R4                                                            
         IC    R4,ACADLNES                                                      
         LA    R3,ACADADD                                                       
         SPACE 1                                                                
ACPAD2   MVC   16(26,R5),0(R3)                                                  
         LA    R3,L'ACADADD(R3)                                                 
         LA    R5,110(R5)                                                       
         BCT   R4,ACPAD2                                                        
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT PROFILE ELEMENT                                           
*                                                                               
ACPPRA   NTR1                                                                   
         USING ACPROFD,R2                                                       
         CLC   RECTYPE,=C'CLIENT '                                              
         BNE   ACPPRA2                                                          
         CLC   ACPRGRUP,SPACES                                                  
         BE    ACPPRA2                                                          
         MVC   0(14,R5),=C'BILLING GROUP='                                      
         MVC   14(3,R5),ACPRGRUP                                                
         LA    R5,110(R5)                                                       
         SPACE 1                                                                
ACPPRA2  OC    ACPRRECV,ACPRRECV                                                
         BZ    *+20                                                             
         MVC   0(19,R5),=C'RECEIVABLE ACCOUNT='                                 
         MVC   19(12,R5),ACPRRECV+3                                             
         LA    R5,110(R5)                                                       
         OC    ACPRCOST,ACPRCOST                                                
         BZ    *+20                                                             
         MVC   0(16,R5),=C'COSTING ACCOUNT='                                    
         MVC   16(12,R5),ACPRCOST+3                                             
         LA    R5,110(R5)                                                       
         CLI   ACPRBILL,0                                                       
         BE    ACPPRA6                                                          
         MVC   0(24,R5),=C'BILLING TYPE=PROGRESSIVE'                            
         CLI   ACPRBILL,C'P'                                                    
         BE    ACPPRA4                                                          
         MVC   13(12,R5),=CL12'CLIENT BILLS'                                    
         CLI   ACPRBILL,C'C'                                                    
         BE    ACPPRA4                                                          
         MVC   13(12,R5),=C'ONE LINE (1)'                                       
         CLI   ACPRBILL,C'1'                                                    
         BE    ACPPRA4                                                          
         MVC   13(12,R5),=CL12'TOTAL'                                           
         CLI   ACPRBILL,C'T'                                                    
         BE    ACPPRA4                                                          
         MVC   13(12,R5),=CL12'UNBILLABLE'                                      
         CLI   ACPRBILL,C'U'                                                    
         BE    ACPPRA4                                                          
         MVC   13(12,R5),=C'SPECIAL'                                            
         EDIT  (4,ACPRBLAM),(10,21(R5)),2,ALIGN=LEFT                            
         CLI   ACPRBILL,C'S'                                                    
         BE    ACPPRA4                                                          
         MVC   13(4,R5),=CL4'FEE'                                               
         MVC   17(15,R5),21(R5)                                                 
         CLI   ACPRBILL,C'F'                                                    
         BE    ACPPRA4                                                          
         MVC   13(15,R5),17(R5)                                                 
         LA    R6,13(R5)                                                        
         CLI   0(R6),C'.'                                                       
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         MVC   0(13,R6),=C' PCT ESTIMATE'                                       
         SPACE 1                                                                
ACPPRA4  LA    R5,110(R5)                                                       
         SPACE 1                                                                
ACPPRA6  CLC   ACPROFFC,SPACES                                                  
         BNH   ACPPRA7                                                          
         MVC   0(7,R5),=C'OFFICE='                                              
         MVC   7(2,R5),ACPROFFC                                                 
         LA    R5,110(R5)                                                       
         SPACE 1                                                                
ACPPRA7  CLC   ACPRUNBL(2),SPACES  CHECK ONLY 2 BYTES BECAUSE OF BO BUG         
         BE    ACPPRAA                                                          
         OC    ACPRUNBL,ACPRUNBL                                                
         BZ    ACPPRAA                                                          
         LA    R6,ACPRUNBL                                                      
         MVC   0(17,R5),=C'UNBILLABLE CODES='                                   
         LA    R7,17(R5)                                                        
         LA    R8,6                                                             
         SPACE 1                                                                
ACPPRA8  MVC   0(2,R7),0(R6)                                                    
         MVI   2(R7),C','                                                       
         CLI   1(R6),C' '                                                       
         BNE   *+12                                                             
         BCTR  R7,0                                                             
         MVC   2(2,R7),=C','                                                    
         LA    R7,3(R7)                                                         
         LA    R6,2(R6)                                                         
         CLI   0(R6),C' '                                                       
         BE    *+8                                                              
         BCT   R8,ACPPRA8                                                       
         BCTR  R7,0                                                             
         CLI   0(R7),C','                                                       
         BNE   *+8                                                              
         MVI   0(R7),C' '                                                       
         LA    R5,110(R5)                                                       
         SPACE 1                                                                
ACPPRAA  OC    ACPRBLPR,ACPRBLPR                                                
         BZ    ACPPRAC                                                          
         CLC   ACPRBLPR,SPACES                                                  
         BE    ACPPRAC                                                          
         MVC   0(17,R5),=C'BILL && EST PRINT='                                  
         MVC   17(50,R5),ACPRBLPR                                               
         LA    R5,110(R5)                                                       
         SPACE 1                                                                
ACPPRAC  CLI   ACPRLEN,105                                                      
         BE    ACPXIT                                                           
         MVC   0(17,R5),=C'EST (OTHER INFO)='                                   
         MVC   17(50,R5),ACPRNARR                                               
         LA    R5,110(R5)                                                       
         CLI   ACPRLEN,155                                                      
         BE    ACPXIT                                                           
         MVC   17(50,R5),ACPRNARR+50                                            
         LA    R5,110(R5)                                                       
         CLI   ACPRLEN,205                                                      
         BE    ACPXIT                                                           
         MVC   17(50,R5),ACPRNARR+100                                           
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT BILL NUMBER ELEMENT                                       
*                                                                               
ACPNUM   NTR1                                                                   
         USING ACNUMD,R2                                                        
         MVC   0(5,R5),=C'MEDIA'                                                
         MVC   PWORK(12),=C'BILL NUMBER='                                       
         MVC   PWORK+12(6),ACNUMAFT                                             
         CLI   ACNUMLEN,14                                                      
         BE    *+20                                                             
         MVC   6(1,R5),ACNUMTYP                                                 
         MVC   8(18,R5),PWORK                                                   
         B     *+16                                                             
         MVC   6(3,R5),=C'ALL'                                                  
         MVC   10(18,R5),PWORK                                                  
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT EXTRA PROFILE ELEMENT                                     
*                                                                               
ACPPRB   NTR1                                                                   
         USING ACXPROFD,R2                                                      
         CP    ACXPDUE,=P'10'                                                   
         BE    ACPPRB2                                                          
         MVC   0(14,R5),=C'DUE DATE DAYS='                                      
         EDIT  (P2,ACXPDUE),(3,14(R5)),ALIGN=LEFT                               
         LA    R5,110(R5)                                                       
ACPPRB2  CP    ACXPOVER,=P'10000'                                               
         BE    ACPPRB4                                                          
         MVC   0(30,R5),=C'PROHIBITIVE PERCENTAGE (OVER)='                      
         EDIT  (P3,ACXPOVER),(6,30(R5)),2,ALIGN=LEFT                            
         LA    R5,110(R5)                                                       
ACPPRB4  CP    ACXPLOW,=P'5000'                                                 
         BE    ACPPRB6                                                          
         MVC   0(25,R5),=C'PROHIBITIVE CASH (UNDER)='                           
         EDIT  (P4,ACXPLOW),(8,25(R5)),2,ALIGN=LEFT                             
         LA    R5,110(R5)                                                       
ACPPRB6  CLI   ACXPSUM,C'Y'                                                     
         BE    *+14                                                             
         MVC   0(10,R5),=C'SUMMARY=NO'                                          
         LA    R5,110(R5)                                                       
         CLI   ACXPNET,C'Y'                                                     
         BNE   *+14                                                             
         MVC   0(07,R5),=C'PAY=NET'                                             
         LA    R5,110(R5)                                                       
         CLI   ACXPDET,C'Y'                                                     
         BE    *+14                                                             
         MVC   0(9,R5),=C'DETAIL=NO'                                            
         LA    R5,110(R5)                                                       
         CLI   ACXPEDET,C'N'                                                    
         BE    *+14                                                             
         MVC   0(13,R5),=C'ESTDETAIL=YES'                                       
         LA    R5,110(R5)                                                       
         CLI   ACXPCD,C'N'                                                      
         BNE   *+14                                                             
         MVC   0(7,R5),=C'DISC=NO'                                              
         LA    R5,110(R5)                                                       
         CLI   ACXPLEN,18                                                       
         BL    ACPXIT                                                           
         CLI   ACXPEST,C'N'                                                     
         BNE   *+14                                                             
         MVC   0(11,R5),=C'ESTIMATE=NO'                                         
         LA    R5,110(R5)                                                       
         TM    ACXPST1,X'80'                                                    
         BZ    *+14                                                             
         MVC   0(6,R5),=C'ETA=NO'                                               
         LA    R5,110(R5)                                                       
         TM    ACXPST1,X'40'                                                    
         BZ    *+14                                                             
         MVC   0(8,R5),=C'ECOMM=NO'                                             
         LA    R5,110(R5)                                                       
         TM    ACXPST1,X'20'                                                    
         BZ    *+14                                                             
         MVC   0(11,R5),=C'TRANSFER=NO'                                         
         LA    R5,110(R5)                                                       
         TM    ACXPST1,X'10'                                                    
         BZ    *+14                                                             
         MVC   0(13,R5),=C'PRODUCTION=NO'                                       
         LA    R5,110(R5)                                                       
         TM    ACXPST1,X'08'                                                    
         BZ    *+14                                                             
         MVC   0(13,R5),=C'POST GROSS+CD'                                       
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              SALES ANALYSIS ELEMENT                                           
         SPACE 2                                                                
ACPSAL   NTR1                                                                   
         USING ACSAND,R2                                                        
         MVC   0(14,R5),=C'SALES ACCOUNT='                                      
         MVC   14(12,R5),ACSACODE+3                                             
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT STATUS ELEMENT                                            
*                                                                               
ACPSTA   NTR1                                                                   
         USING RSTELD,R2                                                        
*        CLI   RSTANAL,C' '                                                     
*        BE    *+20                                                             
*        MVC   0(14,R5),=C'ANALYSIS CODE='                                      
*        MVC   14(1,R5),RSTANAL                                                 
*        LA    R5,110(R5)                                                       
*                                                                               
ACPFLT1  LR    R1,R5                                                            
         CLI   RSTFILT1,C' '                                                    
         BE    ACPFLT2                                                          
         MVC   0(8,R1),=C'FILT1= ,'                                             
         MVC   6(1,R1),RSTFILT1                                                 
         AHI   R1,8                                                             
*                                                                               
ACPFLT2  CLI   RSTFILT2,C' '                                                    
         BE    ACPFLT3                                                          
         MVC   0(8,R1),=C'FILT2= ,'                                             
         MVC   6(1,R1),RSTFILT2                                                 
         AHI   R1,8                                                             
*                                                                               
ACPFLT3  CLI   RSTFILT3,C' '                                                    
         BE    ACPFLT4                                                          
         MVC   0(8,R1),=C'FILT3= ,'                                             
         MVC   6(1,R1),RSTFILT3                                                 
         AHI   R1,8                                                             
*                                                                               
ACPFLT4  CLI   RSTFILT4,C' '                                                    
         BE    ACPFLT5                                                          
         MVC   0(8,R1),=C'FILT4= ,'                                             
         MVC   6(1,R1),RSTFILT4                                                 
         AHI   R1,8                                                             
*                                                                               
ACPFLT5  CLI   RSTFILT5,C' '                                                    
         BE    ACPFLTS                                                          
         MVC   0(8,R1),=C'FILT5= ,'                                             
         MVC   6(1,R1),RSTFILT5                                                 
         AHI   R1,8                                                             
*                                                                               
ACPFLTS  SHI   R1,1                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         AHI   R1,1                                                             
         C     R1,0(R5)                                                         
         BE    ACPCST                                                           
         LA    R5,110(R5)                                                       
*                                                                               
ACPCST   CLC   RSTCOSTG,SPACES                                                  
         BE    ACPXIT                                                           
         CLI   RSTCOSTG,0                                                       
         BE    ACPXIT                                                           
         CLI   RSTCOSTG,C' '                                                    
         BE    ACPXIT                                                           
         MVC   0(14,R5),=C'COSTING GROUP='                                      
         MVC   14(1,R5),RSTCOSTG                                                
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT BUDGET ELEMENT                                            
*                                                                               
ACPBDG2  NTR1                                                                   
         CP    ESTTOT,=P'0'                                                     
         BE    ACPXIT                                                           
         MVC   0(15,R5),=C'ESTIMATE TOTAL='                                     
         EDIT  (P6,ESTTOT),(10,15(R5)),2,ALIGN=LEFT                             
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT JOB ELEMENT                                               
*                                                                               
ACPJOB   NTR1                                                                   
         USING ACJOBD,R2                                                        
         MVC   0(15,R5),=C'JOB CLOSE DATE='                                     
         GOTO1 DATCON,DMCB,(1,ACJBCLOS),(5,15(R5))                              
         CLI   15(R5),C' '                                                      
         BNE   *+8                                                              
         OI    15(R5),X'F0'        FOR UK WHEN DD LESS THAN 10                  
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT POOL ELEMENT                                              
*                                                                               
ACPPOL   NTR1                                                                   
         USING ACPOOLD,R2                                                       
         MVC   0(16,R5),=C'POOL BRAND CODE='                                    
         MVC   16(6,R5),ACPLBRND                                                
         MVI   22(R5),C','                                                      
         MVC   23(22,R5),=C'PRECENTAGE ALLOCATION='                             
         EDIT  (P3,ACPLPCNT),(6,45(R5)),2,ALIGN=LEFT                            
         LA    R5,110(R5)                                                       
         CLC   ACPLRECV,SPACES                                                  
         BE    *+20                                                             
         MVC   0(24,R5),=C'POOL RECEIVABLE ACCOUNT='                            
         MVC   24(12,R5),ACPLRECV+3                                             
         LA    R5,110(R5)                                                       
         CLC   ACPLCOST,SPACES                                                  
         BE    ACPXIT                                                           
         MVC   0(21,R5),=C'POOL COSTING ACCOUNT='                               
         MVC   21(12,R5),ACPLCOST+3                                             
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT COMMENT ELEMENT                                           
*                                                                               
ACPCOM   NTR1                                                                   
         USING ACOMMD,R2                                                        
         SR    R4,R4                                                            
         IC    R4,ACOMLEN                                                       
         SH    R4,=H'5'                                                         
         MVC   0(19,R5),=C'BILL && EST COMMENT='                                
         LA    R6,19(R5)                                                        
         TM    ACOMTYPE,X'C0'                                                   
         BO    ACPCOM2                                                          
         MVC   0(19,R5),SPACES                                                  
         MVC   0(13,R5),=C'BILL COMMENT='                                       
         LA    R6,13(R5)                                                        
         TM    ACOMTYPE,X'80'                                                   
         BO    ACPCOM2                                                          
         MVC   0(17,R5),=C'ESTIMATE COMMENT='                                   
         LA    R6,17(R5)                                                        
         TM    ACOMTYPE,X'40'                                                   
         BO    ACPCOM2                                                          
         MVC   0(18,R5),=CL18'COMMENT='                                         
         LA    R6,8(R5)                                                         
         SPACE 1                                                                
ACPCOM2  EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),ACOMMENT                                                 
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              FORMAT RULES ELEMENT                                             
*                                                                               
ACPRUL   NTR1                                                                   
         USING ACRULED,R2                                                       
         MVC   0(11,R5),=C'RULE MEDIA='                                         
         MVC   11(1,R5),ACRLMED                                                 
         LA    R6,12(R5)                                                        
         CLI   ACRLMED,0                                                        
         BNE   *+14                                                             
         MVC   11(3,R5),=C'ALL'                                                 
         LA    R6,2(R6)                                                         
         MVC   0(11,R6),=C',WORK CODE='                                         
         MVC   11(2,R6),ACRLWORK                                                
         LA    R7,13(R6)                                                        
         CLI   ACRLWORK,0                                                       
         BNE   *+14                                                             
         MVC   11(3,R6),=C'ALL'                                                 
         LA    R7,1(R7)                                                         
         LR    R8,R7                                                            
         CLI   ACRLCOMM,X'FF'                                                   
         BE    ACPRUL6                                                          
         MVC   0(12,R7),=C',COMMISSION='                                        
         LA    R8,12(R7)                                                        
         CP    ACRLCOMM,=P'0'                                                   
         BNE   ACPRUL2                                                          
         MVC   0(4,R8),=C'ZERO'                                                 
         LA    R8,4(R8)                                                         
         B     ACPRUL6                                                          
ACPRUL2  CLI   ACRLLEN,X'0B'                                                    
         BL    ACPRUL4                                                          
         TM    ACRLSTAT,X'80'      4DP                                          
         BZ    ACPRUL4                                                          
         EDIT  (P4,ACRLCOMM),(7,0(R8)),4,ALIGN=LEFT,DROP=1                      
         B     ACPRUL5                                                          
ACPRUL4  EDIT  (P4,ACRLCOMM),(6,0(R8)),2,ALIGN=LEFT                             
ACPRUL5  AR    R8,R0                                                            
         SPACE 1                                                                
ACPRUL6  CLI   ACRLTAX,X'FF'                                                    
         BE    *+16                                                             
         MVC   0(10,R8),=C',TAX CODE='                                          
         MVC   10(1,R8),ACRLTAX                                                 
         LA    R5,110(R5)                                                       
         B     ACPXIT                                                           
         EJECT                                                                  
*              OTHER ROUTINES                                                   
*                                                                               
ACPRINT  NTR1                                                                   
         MVC   HEAD8+46(50),CLINAME                                             
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         SPACE 1                                                                
ACPXIT   XIT1  REGS=(R5)                                                        
         SPACE 1                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
LOOKUP   NTR1  ,                                                                
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ACMACOL                                                       
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   LOOK02              NO                                           
         USING MJETABD,R3                                                       
         ZAP   ESTTOT,MJETVAL                                                   
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
LOOK02   ZAP   ESTTOT,JBCOLVAL                                                  
         B     EXIT                                                             
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE'                                                            
         DROP  R3,R5                                                            
         EJECT                                                                  
*                                                                               
BLOCKEN  DC    H'100'                                                           
         SPACE 1                                                                
         LTORG                                                                  
*              DSECT TO COVER SPACEND                                           
*                                                                               
WRKD     DSECT                                                                  
ABLOCK   DS    A                   A(PRINT BUFFER)                              
SQUASHER DS    V                                                                
         SPACE 1                                                                
ESTTOT   DS    PL6                 TOTAL OF ESTIMATES                           
RECTYPE  DS    CL7                 RECORD TYPE                                  
CLINAME  DS    CL50                CLIENT NAME                                  
PWORK    DS    CL110               *                                            
PWORK2   DS    2CL25               *                                            
         EJECT                                                                  
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT  ON                                                              
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT  ON                                                              
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT  ON                                                              
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
BLOCK    CSECT                                                                  
         DS    100CL110            PRINT BUFFER                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'072ACREP7102 07/23/13'                                      
         END                                                                    
