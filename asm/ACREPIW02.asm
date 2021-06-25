*          DATA SET ACREPIW02  AT LEVEL 012 AS OF 08/17/00                      
*PHASE ACIW02A                                                                  
*INCLUDE DATVAL                                                                 
         TITLE 'ACIW02 - WPP - VALUTECH TAPES'                                  
ACIW02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACIW**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACIWD,RC                                                         
         EJECT                                                                  
*              ROUTINES EXECUTED AT FIRST FOR RUN                               
*                                                                               
RNF00    CLI   MODE,RUNFRST                                                     
         BNE   RQF00                                                            
         OPEN  (INVTAPE,OUTPUT)                                                 
         ZAP   INVOUT,=P'0'        INVOICE RECORDS                              
         ZAP   VNDOUT,=P'0'        VENDOR RECORDS                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT FIRST FOR REQUEST                           
*                                                                               
RQF00    CLI   MODE,REQFRST                                                     
         BNE   PAC00                                                            
         MVI   RCSUBPRG,0          DEFAULT IS INVOICE DETAIL                    
         CLI   QOPT1,C'V'                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,1          BUT CAN PRINT VENDOR RECORDS                 
         ZAP   ACCTOT,=P'0'                                                     
         ZAP   LDGTOT,=P'0'                                                     
         CLI   QLEDGER,C'V'        VENDOR LEDGER                                
         BNE   RQF03                                                            
         OPEN  (VNDTAPE,OUTPUT)                                                 
*                                                                               
RQF03    MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STRT)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT FIRST FOR ACCOUNT                           
*                                                                               
PAC00    CLI   MODE,PROCACC                                                     
         BNE   SBF00                                                            
         ZAP   ACCTOT,=P'0'                                                     
         MVC   VTREC,SPACES        INITIALIZE VENDOR AREA                       
         MVC   ACTV,C'N'                                                        
         L     R3,ADACC                                                         
         MVC   VTACCT,1(R3)                                                     
         MVC   WORK(36),SPACES                                                  
         L     R4,ADACCNAM                                                      
         USING ACNAMED,R4                                                       
         SR    R2,R2                                                            
         IC    R2,ACNMLEN                                                       
         SH    R2,=H'3'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         MVC   VTNME,WORK             ACCOUNT NAME                              
         ZAP   VNDYTD,=P'0'           INIT VENDOR YTD (1990) TOTALS             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED FOR HISTORY RECORDS                            
*                                                                               
SBF00    CLI   MODE,SBACFRST                                                    
         BNE   PTN00                                                            
         L     R4,ADSUBAC                                                       
         SR    R1,R1                                                            
*                                                                               
SBF03    CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'45'         GET HISTORIES                                
         BNE   SBF05                                                            
         USING TRHISTD,R4                                                       
         CLI   TRHSYEAR,X'90'      FOR 1990                                     
         BNE   SBF05                                                            
         AP    VNDYTD,TRHSDR       PAYMENTS                                     
*                                                                               
SBF05    IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     SBF03                                                            
         EJECT                                                                  
*              ROUTINES EXECUTED FOR TRANSACTION RECORDS                        
*                                                                               
PTN00    CLI   MODE,PROCTRNS                                                    
         BNE   ACL00                                                            
         L     R3,ADTRANS                                                       
         SH    R3,DATADISP                                                      
         USING ACKEYD,R3                                                        
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   XIT                 SKIP PEELED                                  
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         TM    TRNSSTAT,X'20'      SEE IF POSTING IS A REVERSAL                 
         BO    XIT                 IF YES, XIT                                  
*                                                                               
         TM    TRNSSTAT,X'80'      IS IT A DEBIT                                
         BO    PTN15                                                            
         MVC   LSTKEY,ACKEYACC     SAVE ACCT/CON/DATE/REF                       
         MVC   ITREC,SPACES                                                     
         MVC   ITACCT,VTACCT       SET VENDOR ACCOUNT                           
         ZAP   NET,TRNSAMNT        CREDIT TRANSACTIONS FOR PAYABLES             
         ZAP   GRS,TRNSAMNT                                                     
         ZAP   CD,=P'0'                                                         
         MVC   ITINVN(6),TRNSREF   INVOICE NUMBER FOR PROD                      
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(X'20',WORK)                            
         MVC   ITINVD(4),WORK+2    MMDD                                         
         MVC   ITINVD+4(2),WORK    YY                                           
         LR    R5,R4                                                            
         SR    R1,R1                                                            
*                                                                               
PTN07    IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLI   0(R5),0                                                          
         BE    XIT                                                              
         CLI   0(R5),X'50'                                                      
         BNE   PTN09                                                            
         USING TRCASHD,R5                                                       
         CLI   TRCSTYPE,C'D'       GET C.D.                                     
         BNE   PTN07                                                            
         ZAP   CD,TRCSAMNT                                                      
         B     XIT                                                              
*                                                                               
PTN09    CLI   0(R5),X'46'         EXTRA PAY ELEMENT                            
         BNE   PTN07                                                            
         USING TRPAYD,R5                                                        
         MVC   ITINVN(14),TRPYINV  INVOICE NUMBER                               
         OC    ITINVN,SPACES                                                    
         ZAP   CD,TRPYCD           AND C.D.                                     
         OC    TRPYPER,TRPYPER     CHECK THE PERIOD FOR MEDIA                   
         BZ    XIT                                                              
         LA    RF,TRPYPER+6        FIRST TRY END DATE                           
         CLI   0(RF),C'-'                                                       
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         LA    RF,TRPYPER          THEN START                                   
         BNH   XIT                                                              
         CLC   4(2,RF),=C'00'                                                   
         BH    *+10                                                             
         MVC   4(2,RF),=C'01'      FORCE DAY                                    
         MVC   ITINVD(4),2(RF)     MMDD                                         
         MVC   ITINVD+4(2),0(RF)   YY                                           
         B     XIT                                                              
*                                                                               
PTN15    EQU   *                   DEBIT TRANSACTIONS FOR PAYABLES              
         CLC   ACKEYACC(41),LSTKEY SAME KEY?                                    
         BNE   PTNX                                                             
         GOTO1 =V(DATVAL),DMCB,(0,TRNSNARR+6),WORK                              
         OC    DMCB(4),DMCB                                                     
         BZ    PTNX                NO PAID DATE                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         CLC   WORK+6(2),STRT                                                   
         BL    PTNX                                                             
         CLC   WORK+6(2),END                                                    
         BH    PTNX                                                             
         GOTO1 DATCON,DMCB,(1,WORK+6),(X'20',WORK)                              
         MVC   ITPDD(4),WORK+2     MMDD PAID DATE                               
         MVC   ITPDD+4(2),WORK     YY                                           
         AP    GRS,CD                                                           
         CP    GRS,=P'0'                                                        
         BNE   *+8                                                              
         MVI   GRS+5,X'0F'                                                      
         UNPK  ITGRS,GRS                                                        
         CP    CD,=P'0'                                                         
         BNE   *+8                                                              
         MVI   CD+5,X'0F'                                                       
         UNPK  ITCD,CD                                                          
         CP    NET,=P'0'                                                        
         BNE   *+8                                                              
         MVI   NET+5,X'0F'                                                      
         UNPK  ITNET,NET                                                        
         MVI   ITTRG,C'I'                                                       
         MVI   ACTV,C'Y'           CURRENT ACTIVITY                             
         AP    INVOUT,=P'1'                                                     
         PUT   INVTAPE,ITREC                                                    
*                                  SET PRINT LINE                               
         MVC   IPACCT,ITACCT       ACCOUNT                                      
         MVC   IPINVN,ITINVN       INVOICE NUMBER                               
         EDIT  GRS,(16,IPGRS),2,MINUS=YES                                       
         EDIT  CD,(16,IPCD),2,MINUS=YES                                         
         EDIT  NET,(16,IPNET),2,MINUS=YES                                       
         MVC   IPINVD,ITINVD       INVOICE DATE                                 
         MVC   IPPDD,ITPDD         PAID DATE                                    
         MVC   IPVCH,ITVCH                                                      
         MVC   IPTRG,ITTRG                                                      
         MVC   IPTRM,ITTRM                                                      
         AP    ACCTOT,NET                                                       
         CLI   QOPT1,C'I'          PRINT INVOICE DETAIL                         
         BNE   PTNX                                                             
         GOTO1 ACREPORT                                                         
*                                                                               
PTNX     DS    0H                                                               
         MVC   LSTKEY,SPACES        CLEAR SAVED KEY                             
         MVC   P,SPACES                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT LAST FOR ACCOUNT                            
*                                                                               
ACL00    CLI   MODE,ACCLAST                                                     
         BNE   LGL00                                                            
         MVC   P,SPACES                                                         
         AP    LDGTOT,ACCTOT                                                    
         CLI   QOPT1,C'I'                                                       
         BNE   ACL01                                                            
         CP    ACCTOT,=P'0'                                                     
         BE    ACL01                                                            
         EDIT  ACCTOT,(16,IPNET),2,MINUS=YES                                    
         MVC   IPGRS(13),=C'ACCOUNT TOTAL'                                      
         GOTO1 ACREPORT                                                         
*                                                                               
ACL01    CLI   QLEDGER,C'V'                                                     
         BNE   XIT                                                              
*        CLI   ACTV,C'Y'                                                        
*        BNE   XIT                 NO CURRENT ACTIVITY                          
         ZAP   CDR,=P'0'           ASSUME NO CD RATE                            
         CP    VNDYTD,=P'0'                                                     
         BE    XIT                                                              
         UNPK  VTPRH,VNDYTD        YTD PURCHASES                                
         L     R3,ADACC                                                         
         LA    R4,ACRECORD                                                      
         SR    R1,R1                                                            
*                                                                               
ACL03    IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    ACL09                                                            
         CLI   0(R4),X'38'         DISCOUNT ELEMENT                             
         BE    ACL05                                                            
         CLI   0(R4),X'23'         TAX ID                                       
         BE    ACL07                                                            
         B     ACL03                                                            
*                                                                               
         USING ACVATD,R4                                                        
ACL05    SR    RF,RF                                                            
         ICM   RF,3,ACVTRATE        CD RATE                                     
         CVD   RF,DUB                                                           
         MP    DUB,=P'10'          3 DECIMAL PLACES                             
         ZAP   CDR,DUB                                                          
         B     ACL03                                                            
*                                                                               
         USING ACOTHERD,R4                                                      
ACL07    CLI   ACOTPROF,C'I'       TAX ID                                       
         BNE   ACL03                                                            
         MVC   VTID(9),ACOTNUM                                                  
         B     ACL03                                                            
*                                                                               
ACL09    UNPK  VTCD,CDR            CD RATE TO TAPE                              
         MVC   VTDDAY,=6C'0'                                                    
         MVC   VTNDAY,=6C'0'                                                    
         AP    VNDOUT,=P'1'                                                     
         PUT   VNDTAPE,VTREC                                                    
*                                                                               
         MVC   P,SPACES                                                         
         MVC   VPACCT,VTACCT                                                    
         MVC   VPNME,VTNME                                                      
         EDIT  VNDYTD,(15,VPPRH),2,MINUS=YES                                    
         EDIT  CDR,(6,VPCD),3                                                   
         MVC   VPDTYP,VTDTYP                                                    
         MVC   VPDDAY,VTDDAY                                                    
         MVC   VPNTYP,VTNTYP                                                    
         MVC   VPNDAY,VTNDAY                                                    
         MVC   VPID,VTID                                                        
         CLI   QOPT1,C'V'                                                       
         BNE   ACLX                                                             
         GOTO1 ACREPORT                                                         
*                                                                               
ACLX     MVC   P,SPACES                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT LAST FOR ACCOUNT                            
*                                                                               
LGL00    CLI   MODE,LEDGLAST                                                    
         BNE   RQL00                                                            
         CLI   QOPT1,C'I'                                                       
         BNE   XIT                                                              
         CP    LDGTOT,=P'0'                                                     
         BE    XIT                                                              
         EDIT  LDGTOT,(16,IPNET),2,MINUS=YES                                    
         MVC   IPGRS(13),=C'LEDGER TOTAL '                                      
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT LAST FOR REQUEST                            
*                                                                               
RQL00    CLI   MODE,REQLAST                                                     
         BNE   RNL00                                                            
         CLI   QLEDGER,C'V'                                                     
         BNE   XIT                                                              
         CLOSE (VNDTAPE)                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES EXECUTED AT LAST FOR RUN                                
*                                                                               
RNL00    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         CLOSE (INVTAPE)                                                        
         MVC   P+1(20),=CL20'INVOICE RECORDS'                                   
         EDIT  INVOUT,(7,P+22)                                                  
         GOTO1 ACREPORT                                                         
         MVC   P+1(20),=CL20'VENDOR RECORDS'                                    
         EDIT  VNDOUT,(7,P+22)                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS                                                        
         PRINT GEN                                                              
VNDTAPE  DCB   DSORG=PS,MACRF=PM,DDNAME=VNDTAPE,BLKSIZE=870,LRECL=87            
INVTAPE  DCB   DSORG=PS,MACRF=PM,DDNAME=INVTAPE,BLKSIZE=1090,LRECL=109          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
*                                                                               
ACIWD    DSECT                                                                  
ITREC    DS    0CL(ITLN)          INVOICE TAPE RECORD                           
ITBGN    DS    0C                                                               
ITACCT   DS    CL16                U/L ACCOUNT CODE                             
ITINVN   DS    CL16                INVOICE NUMBER                               
ITGRS    DS    CL13                GROSS                                        
ITCD     DS    CL13                CASH DISCOUNT                                
ITNET    DS    CL13                NET AMOUNT                                   
ITINVD   DS    CL6                 INVOICE DATE - MMDDYY                        
ITPDD    DS    CL6                 PAID DATE - MMDDYY                           
ITVCH    DS    CL16                VOUCHER(NOT USED)                            
ITTRG    DS    CL1                 TRIGGER CODE, ALWAYS AN I                    
ITTRM    DS    CL9                 TERM CODE                                    
ITLN     EQU   *-ITBGN                                                          
         EJECT                                                                  
VTREC    DS    0CL(VTLN)           VENDOR TAPE RECORD                           
VTBGN    DS    0C                                                               
VTACCT   DS    CL16                U/L ACCOUNT CODE                             
VTNME    DS    CL32                ACCOUNT NAME                                 
VTPRH    DS    CL13                PURCHASES - TOTAL 1990                       
VTCD     DS    CL6                 CASH DISCOUNT  999.999                       
VTDTYP   DS    CL1                 DISCOUNT TYPE                                
VTDDAY   DS    CL7                 DISCOUNT DAYS                                
VTNTYP   DS    CL1                 NET DAYS TYPE                                
VTNDAY   DS    CL4                 NET DAYS                                     
VTID     DS    CL10                VENDOR ID                                    
VTLN     EQU   *-VTBGN                                                          
*                                                                               
STRT     DS    CL3                 START DATE                                   
END      DS    CL3                 END DATE                                     
ACTV     DS    CL1                 CURRENT ACTIVITY                             
INVOUT   DS    PL4                 INVOICE RECORD COUNT                         
VNDOUT   DS    PL4                 VENDOR RECORD COUNT                          
VNDYTD   DS    PL6                 VENDOR 1990 PAYMENTS                         
NET      DS    PL6                                                              
CD       DS    PL6                                                              
GRS      DS    PL6                                                              
CDR      DS    PL5                 CD RATE                                      
LSTKEY   DS    CL41                KEY OF LAST CREDIT                           
ACCTOT   DS    PL6                                                              
LDGTOT   DS    PL6                                                              
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*                                                                               
         ORG   P+1                                                              
IPREC    DS    0CL(IPLN)           INVOICE PRINT LINE                           
IPBGN    DS    0C                                                               
IPACCT   DS    CL16                U/L ACCOUNT CODE                             
         DS    CL1                                                              
IPINVN   DS    CL16                INVOICE NUMBER                               
         DS    CL1                                                              
IPGRS    DS    CL15                GROSS                                        
         DS    CL1                                                              
IPCD     DS    CL15                CASH DISCOUNT                                
         DS    CL1                                                              
IPNET    DS    CL15                NET AMOUNT                                   
         DS    CL3                                                              
IPINVD   DS    CL6                 INVOICE DATE - MMDDYY                        
         DS    CL3                                                              
IPPDD    DS    CL6                 PAID DATE - MMDDYY                           
         DS    CL1                                                              
IPVCH    DS    CL16                VOUCHER(NOT USED)                            
         DS    CL3                                                              
IPTRG    DS    CL1                 TRIGGER CODE, ALWAYS AN I                    
         DS    CL1                                                              
IPTRM    DS    CL9                 TERM CODE                                    
IPLN     EQU   *-IPBGN                                                          
*                                                                               
         ORG   P+1                                                              
VPREC    DS    0CL(VPLN)           VENDOR PRINT LINE                            
VPBGN    DS    0C                                                               
VPACCT   DS    CL16                U/L ACCOUNT CODE                             
         DS    CL1                                                              
VPNME    DS    CL32                ACCOUNT NAME                                 
         DS    CL1                                                              
VPPRH    DS    CL15                PURCHASES - TOTAL 1990                       
         DS    CL1                                                              
VPCD     DS    CL6                 CASH DISCOUNT  %                             
         DS    CL3                                                              
VPDTYP   DS    CL1                 DISCOUNT TYPE                                
         DS    CL4                                                              
VPDDAY   DS    CL4                 DISCOUNT DAYS                                
         DS    CL2                                                              
VPNTYP   DS    CL1                 NET DAYS TYPE                                
         DS    CL4                                                              
VPNDAY   DS    CL4                 NET DAYS                                     
         DS    CL1                                                              
VPID     DS    CL10                VENDOR ID                                    
VPLN     EQU   *-VPBGN                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREPIW02 08/17/00'                                      
         END                                                                    
