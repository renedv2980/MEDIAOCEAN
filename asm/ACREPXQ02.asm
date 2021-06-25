*          DATA SET ACREPXQ02  AT LEVEL 004 AS OF 05/01/02                      
*PHASE ACXQ02A,+0                                                               
         TITLE 'CHECK ACC VS ACC/OFF BALANCE ELEMENTS'                          
ACXQ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXQ**                                                       
*                                                                               
         USING ACWORKD,RA                                                       
         USING ACXQD,RC                                                         
         USING AMTTABD,R7                                                       
         LA    RC,SPACEND                                                       
         CLI   MODE,RUNFRST                                                     
         BNE   RF10                                                             
         ZAP   ERROR#,=P'0'                                                     
         ZAP   DRRUN,=P'0'         CLEAR ACCUMS FOR RUN                         
         ZAP   CRRUN,=P'0'                                                      
         L     RE,=A(IO)                                                        
         ST    RE,AIO                                                           
         L     RE,=A(BALTAB)                                                    
         ST    RE,ABALTBL                                                       
         B     XIT                                                              
         EJECT                                                                  
RF10     DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   PA10                                                             
*                                                                               
         USING CPYELD,R5                                                        
         L     R5,ADCMPEL                                                       
         MVI   OFFICE2,C'N'        NOT 2 BYTE OFFICE                            
         TM    CPYSTAT4,CPYSOFF2   2 BYTE OFFICE SUPPORTED?                     
         BZ    XIT                                                              
         MVI   OFFICE2,C'Y'                                                     
         DROP  R5                                                               
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         ZAP   DRREQ,=P'0'         CLEAR ACCUMS FOR REQUEST                     
         ZAP   CRREQ,=P'0'                                                      
         MVI   FCRDOFA,YES         PROCESS OFFFICE ACCOUNT                      
*                                                                               
         USING ACMD,RE                                                          
         L     RE,AMONACC         CLEAR FISCAL START SO MONACC                  
         XC    ACMFDTE,ACMFDTE    WON'T FILTER TRANS FOR P&L LEDGERS            
         MVC   ADACCOF,ACMAOFA                                                  
         DROP  RE                                                               
*                                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
         USING ABLELD,R6                                                        
PA10     DS    0H                                                               
         CLI   MODE,PROCACC                                                     
         BNE   AO10                                                             
         CLI   OFFICE2,C'Y'                                                     
         BNE   XIT                                                              
         ZAP   DRTRN,=P'0'                                                      
         ZAP   CRTRN,=P'0'                                                      
         ZAP   DRPEEL,=P'0'                                                     
         ZAP   CRPEEL,=P'0'                                                     
         L     R6,ADACCBAL         ADDR OF BAL ELEM (X'32')                     
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'                CANNOT BE ZERO                               
         CLI   0(R6),X'32'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST HAVE 32 ELEMENT                         
         ZAP   BFTOT,ABLFRWD                                                    
         ZAP   DRTOT,ABLDR                                                      
         ZAP   CRTOT,ABLCR                                                      
*                                                                               
         L     RE,ABALTBL                                                       
         ST    RE,ABALETRY                                                      
         L     RF,=AL4(OFF#*AMTTLNQ)                                            
         XCEFL                                                                  
         XC    CURROFF,CURROFF     CLEAR CURRENT OFFICE                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* PROCOFA                                                            *          
*--------------------------------------------------------------------*          
         USING OFARECD,R3                                                       
         USING ABLELD,R6                                                        
AO10     DS    0H                                                               
         CLI   MODE,PROCOFA                                                     
         BNE   TX10                                                             
         CLI   OFFICE2,C'Y'        2 BYTE OFFICE?                               
         BNE   XIT                                                              
         L     R6,ADACCOF                                                       
*                                                                               
         SR    R1,R1                                                            
         AH    R6,DATADISP                                                      
AO20     CLI   0(R6),0                                                          
         BE    AO90                                                             
         CLI   0(R6),ABLELQ        X'32'                                        
         BE    AO50                                                             
         IC    R1,1(,R6)                                                        
         AR    R6,R1                                                            
         B     AO20                                                             
*                                                                               
AO50     L     R7,ABALETRY                                                      
         CLI   0(R7),0                                                          
         BE    *+8                                                              
         LA    R7,AMTTLNQ(,R7)                                                  
         ST    R7,ABALETRY                                                      
         MVC   AMTTOFF,OFAKOFF                                                  
         ZAP   AMTBBF,ABLFRWD                                                   
         ZAP   AMTBDR,ABLDR                                                     
         ZAP   AMTBCR,ABLCR                                                     
         ZAP   AMTTDR,=P'0'                                                     
         ZAP   AMTTCR,=P'0'                                                     
         ZAP   AMTPDR,=P'0'                                                     
         ZAP   AMTPCR,=P'0'                                                     
AO90     B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*  PROCTRNS                                                          *          
*--------------------------------------------------------------------*          
         USING TRNELD,R6                                                        
TX10     DS    0H                                                               
         CLI   MODE,PROCTRNS                                                    
         BNE   AL10                                                             
         CLI   OFFICE2,C'Y'        2 BYTE OFFICE?                               
         BNE   XIT                                                              
*                                                                               
         USING TRSELD,R3                                                        
         SR    R1,R1                                                            
         L     R3,ADTRANS                                                       
         MVI   PEELED,NO                                                        
TX10A    CLI   0(R3),0                                                          
         BE    TX10C                                                            
         CLI   TRSEL,TRSELQ        X'60' STATUS ELEMENT                         
         BE    TX10B                                                            
         IC    R1,1(,R3)                                                        
         AR    R3,R1                                                            
         B     TX10A                                                            
*                                                                               
TX10B    OC    TRSPDAT,TRSPDAT                                                  
         BZ    *+8                                                              
         MVI   PEELED,YES                                                       
*                                                                               
TX10C    L     R6,ADTRANS          POINT TO TRANSACTION ELEM (X'44')            
         CLI   TRNEL,TRNELQ        X'44'                                        
         BNE   XIT                                                              
*                                                                               
         USING TRNRECD,R3                                                       
         LR    R3,R6                                                            
         SH    R3,DATADISP         POINT TO KEY                                 
         L     R7,ABALETRY         LAST  ENTRY                                  
         CLC   AMTTOFF,TRNKOFF     ARE   OFFICES THE SAME                       
         BE    TX15                                                             
*                                                                               
         L     R7,ABALTBL                                                       
TX11     CLI   0(R7),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   AMTTOFF,TRNKOFF                                                  
         BE    TX15                                                             
         LA    R7,AMTTLNQ(,R7)                                                  
         B     TX11                                                             
*                                                                               
TX15     MVC   CURROFF,TRNKOFF                                                  
         ST    R7,ABALETRY                                                      
         TM    TRNSTAT,X'80'       1=DR  0=CR                                   
         BO    TX18                                                             
         AP    CRTRN,TRNAMNT       ADD TO ACCOUNT TOTALS                        
         AP    AMTTCR,TRNAMNT                                                   
         CLI   PEELED,NO           MARKED PEELED?                               
         BE    TX19                                                             
         AP    CRPEEL,TRNAMNT      ADD TO ACCOUNT TOTALS                        
         AP    AMTPCR,TRNAMNT                                                   
         B     TX19                                                             
*                                                                               
TX18     AP    DRTRN,TRNAMNT                                                    
         AP    AMTTDR,TRNAMNT                                                   
         CLI   PEELED,NO           MARKED PEELED?                               
         BE    TX19                                                             
         AP    DRPEEL,TRNAMNT                                                   
         AP    AMTPDR,TRNAMNT                                                   
*                                                                               
TX19     B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
* ACCLAST                                                            *          
*--------------------------------------------------------------------*          
         SPACE 1                                                                
         USING AMTTABD,R7                                                       
AL10     DS    0H                                                               
         CLI   MODE,ACCLAST                                                     
         BNE   RQ10                                                             
         CLI   OFFICE2,C'Y'        2 BYTE OFFICE?                               
         BNE   XIT                                                              
*                                                                               
         MVI   ERRORFG,NO                                                       
         L     R7,ABALTBL                                                       
         ZAP   TMPDRTOT,DRTOT                                                   
         ZAP   TMPCRTOT,CRTOT                                                   
         ZAP   TMPBFTOT,BFTOT                                                   
AL15     CLI   0(R7),0             EOT                                          
         BE    AL20                                                             
         SP    TMPDRTOT,AMTBDR                                                  
         SP    TMPCRTOT,AMTBCR                                                  
         SP    TMPBFTOT,AMTBBF                                                  
         LA    R7,AMTTLNQ(,R7)                                                  
         B     AL15                                                             
*                                                                               
AL20     CP    TMPDRTOT,=P'0'                                                   
         BNE   AL22                                                             
         CP    TMPCRTOT,=P'0'                                                   
         BNE   AL22                                                             
         CP    TMPBFTOT,=P'0'                                                   
         BE    AL24                                                             
AL22     AP    ERROR#,=P'1'                                                     
         MVI   ERRORFG,YES                                                      
*                                                                               
AL24     L     R3,ADACC            POINT TO ACCOUNT                             
         MVC   P+1(12),3(R3)                                                    
         L     R4,ADACCNAM         POINT TO ACCOUNT NAME                        
         LA    R5,P+17                                                          
         BAS   RE,NAMOUT                                                        
         CLI   ERRORFG,YES                                                      
         BNE   *+10                                                             
         MVC   P+45(13),=C'*** ERROR ***'                                       
         GOTO1 ACREPORT                                                         
*                                                                               
         CURED (P8,DRTOT),(15,P+5),2,MINUS=YES                                  
         MVC   P+20(2),=C'DR'                                                   
         CURED (P8,CRTOT),(15,P+25),2,MINUS=YES                                 
         MVC   P+40(2),=C'CR'                                                   
         CURED (P8,BFTOT),(15,P+45),2,MINUS=YES                                 
         MVC   P+60(2),=C'BF'                                                   
         MVC   P+70(10),=C'ACCOUNT   '                                          
         GOTO1 ACREPORT                                                         
*                                                                               
         CURED (P8,DRTRN),(15,P+5),2,MINUS=YES                                  
         MVC   P+20(2),=C'DR'                                                   
         CURED (P8,CRTRN),(15,P+25),2,MINUS=YES                                 
         MVC   P+40(2),=C'CR'                                                   
         MVC   P+70(11),=C'TRANS TOTAL'                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         CURED (P8,DRPEEL),(15,P+5),2,MINUS=YES                                 
         MVC   P+20(2),=C'DR'                                                   
         CURED (P8,CRPEEL),(15,P+25),2,MINUS=YES                                
         MVC   P+40(2),=C'CR'                                                   
         MVC   P+70(12),=C'PEELED TOTAL'                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         L     R7,ABALTBL                                                       
AL25     CLI   0(R7),0             EOT                                          
         BE    AL50                                                             
         MVC   P,SPACES                                                         
         MVC   P+1(2),AMTTOFF                                                   
         CURED (P8,AMTTDR),(15,P+5),2,MINUS=YES                                 
         MVC   P+20(2),=C'DR'                                                   
         CURED (P8,AMTTCR),(15,P+25),2,MINUS=YES                                
         MVC   P+40(2),=C'CR'                                                   
         MVC   P+70(10),=C'TRANACTION'                                          
         GOTO1 ACREPORT                                                         
*                                                                               
         CURED (P8,AMTPDR),(15,P+5),2,MINUS=YES                                 
         MVC   P+20(2),=C'DR'                                                   
         CURED (P8,AMTPCR),(15,P+25),2,MINUS=YES                                
         MVC   P+40(2),=C'CR'                                                   
         MVC   P+70(12),=C'PEELED TRANS'                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         CURED (P8,AMTBDR),(15,P+5),2,MINUS=YES                                 
         MVC   P+20(2),=C'DR'                                                   
         CURED (P8,AMTBCR),(15,P+25),2,MINUS=YES                                
         MVC   P+40(2),=C'CR'                                                   
         CURED (P8,AMTBBF),(15,P+45),2,MINUS=YES                                
         MVC   P+60(2),=C'BF'                                                   
         MVC   P+70(10),=C'ACT OFFICE'                                          
         GOTO1 ACREPORT                                                         
*                                                                               
         ZAP   PKDIFF,AMTTDR       INIT THE DIFFERENCE                          
         SP    PKDIFF,AMTBDR       TAKE BAL AWAY FROM TRN                       
         BZ    *+10                                                             
         MVC   P+5(11),=C'***ERROR***'                                          
         CURED (P8,PKDIFF),(15,P+5),2,MINUS=YES                                 
         MVC   P+20(4),=C'DIFF'                                                 
         ZAP   PKDIFF,AMTTCR       INIT THE DIFFERENCE                          
         SP    PKDIFF,AMTBCR       TAKE BAL AWAY FROM TRN                       
         BZ    *+10                                                             
         MVC   P+5(11),=C'***ERROR***'                                          
         CURED (P8,PKDIFF),(15,P+25),2,MINUS=YES                                
         MVC   P+40(4),=C'DIFF'                                                 
         MVC   P+70(12),=C'TRN/BAL DIFF'                                        
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R7,AMTTLNQ(,R7)     BUMP TO NEXT OFFICE                          
         B     AL25                LOOP BACK UNTIL ALL DONE                     
*                                                                               
AL50     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*&&DO                                                                           
         L     R4,ADACC            POINT TO ACCOUNT                             
         MVC   P+1(12),3(R4)                                                    
         L     R4,ADACCNAM         POINT TO ACCOUNT NAME                        
         LA    R5,P+17                                                          
         BAS   RE,NAMOUT                                                        
         LA    R4,DRTOT                                                         
         BAS   RE,EDAMNT                                                        
         GOTO1 MYREPORT                                                         
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
RQ10     DS    0H                                                               
         CLI   MODE,REQLAST                                                     
         BNE   RL10                                                             
         CLI   OFFICE2,C'Y'        2 BYTE OFFICE?                               
         BNE   XIT                                                              
         CP    ERROR#,=P'0'                                                     
         BE    XIT                                                              
         MVC   P+2(7),=C'ERRORS='                                               
         CURED (P4,ERROR#),(10,P+8),0,MINUS=YES                                 
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*&&DO                                                                           
         CLC   DRREQ(12),=2PL8'0'                                               
         BE    XIT                 NO TRANSACTIONS FOR REQUEST                  
         GOTO1 ACREPORT                                                         
         AP    DRRUN,DRREQ         ROLL FWRD REQUEST TOTALS                     
         AP    CRRUN,CRREQ                                                      
         LA    R4,DRREQ                                                         
         BAS   RE,EDAMNT                                                        
         MVC   P+40(18),=C'TOTALS FOR REQUEST'                                  
         MVI   SPACING,3                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
RL10     DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         CLI   OFFICE2,C'Y'        2 BYTE OFFICE?                               
         BNE   XIT                                                              
         B     XIT                                                              
*&&DO                                                                           
         CLC   DRRUN(12),=2PL8'0'                                               
         BE    XIT                 NO TRANSACTIONS FOR RUN                      
         GOTO1 ACREPORT                                                         
         LA    R4,DRRUN                                                         
         BAS   RE,EDAMNT                                                        
         MVC   P+40(14),=C'TOTALS FOR RUN'                                      
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*&&                                                                             
         EJECT                                                                  
*              VARIOUS AIDS ETC                                                 
         SPACE 1                                                                
GETEL    GETEL R3,DATADISP,ELCODE                                               
         SPACE 2                                                                
         USING NAMELD,R4                                                        
NAMOUT   DS    0H                                                               
         ZIC   RF,NAMLN                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),NAMEREC                                                  
         BR    RE                                                               
         SPACE 2                                                                
*&&DO                                                                           
EDAMNT   DS    0H                                                               
         CP    0(8,R4),=P'0'                                                    
         BE    EDAMNT2                                                          
         CURED (P8,0(R4)),(15,P+60),2,MINUS=YES                                 
         ZAP   0(8,R4),=P'0'                                                    
EDAMNT2  CP    8(8,R4),=P'0'                                                    
         BER   RE                                                               
         CURED (P8,8(R4)),(15,P+78),2,MINUS=YES                                 
         ZAP   8(8,R4),=P'0'                                                    
         BR    RE                                                               
         SPACE 2                                                                
MYREPORT NTR1                                                                   
         MVC   HEAD5+10(1),QLEDGER                                              
         LA    R5,HEAD5+12                                                      
         L     R4,ADLDGNAM                                                      
         BAS   RE,NAMOUT                                                        
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
*&&                                                                             
         EJECT                                                                  
ACCFIL   DC    CL8'ACCOUNT'                                                     
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
OFF#     EQU   200                                                              
IO       DS    2000C               IO AREA FOR OFFICE ACCOUNTS                  
BALTAB   DS    CL(OFF#*AMTTLNQ)    X'32'       TOTALS FOR ACC/OFF               
         EJECT                                                                  
**********************************************************************          
*  DSECT FOR PROGRAM                                                            
**********************************************************************          
ACXQD    DSECT                                                                  
AIO      DS    A                                                                
ABALTBL  DS    A                                                                
ABALETRY DS    A                                                                
ADACCOF  DS    A                                                                
*                                                                               
BFTOT    DS    PL8                                                              
DRTOT    DS    PL8                 ACCUMS                                       
CRTOT    DS    PL8                                                              
*                                                                               
BFTRN    DS    PL8                                                              
DRTRN    DS    PL8                 ACCUMS                                       
CRTRN    DS    PL8                                                              
*                                                                               
BFPEEL   DS    PL8                                                              
DRPEEL   DS    PL8                 ACCUMS                                       
CRPEEL   DS    PL8                                                              
*                                                                               
BFAOF    DS    PL8                                                              
DRAOF    DS    PL8                 ACCUMS                                       
CRAOF    DS    PL8                                                              
*                                                                               
DRREQ    DS    PL8                                                              
CRREQ    DS    PL8                                                              
*                                                                               
DRRUN    DS    PL8                                                              
CRRUN    DS    PL8                                                              
*                                                                               
TMPBFTOT DS    PL8                                                              
TMPDRTOT DS    PL8                 ACCUMS                                       
TMPCRTOT DS    PL8                                                              
*                                                                               
PKDIFF   DS    PL8                 DIFFERENCE IF ANY BETWEEN TRN/BAL            
*                                                                               
ERROR#   DS    PL4                                                              
*                                                                               
TODAY2   DS    CL2                 FROM DATECARD                                
TODAY2P  DS    CL2                 FROM PEEL ELEMENT (X'33')                    
ELCODE   DS    CL1                                                              
ACCTSW   DS    CL1                 FLAG FOR ACCOUNT PRINT                       
OFFICE2  DS    CL1                 2 BYTE OFFICE                                
PEELED   DS    CL1                 YES/NO                                       
ERRORFG  DS    CL1                 YES/NO                                       
CURROFF  DS    CL2                 CURRENT OFFICE                               
TABPOINT DS    A                   POINTER TO AMOUNT TABLE                      
SVKEY    DS    CL42                                                             
         EJECT                                                                  
AMTTABD  DSECT                                                                  
AMTTOFF  DS    CL2                 OFFICE                                       
AMTTDR   DS    PL8                 DEBIT                                        
AMTTCR   DS    PL8                 CREDIT                                       
AMTBBF   DS    PL8                                                              
AMTBDR   DS    PL8                 DEBIT                                        
AMTBCR   DS    PL8                 CREDIT                                       
AMTPDR   DS    PL8                 DEBIT                                        
AMTPCR   DS    PL8                 CREDIT                                       
AMTTLNQ  EQU   *-AMTTOFF                                                        
         EJECT                                                                  
*        ACREPWORKD                                                             
*        ACGENFILE                                                              
*        ACGENMODES                                                             
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPXQ02 05/01/02'                                      
         END                                                                    
