*          DATA SET ACBAT19    AT LEVEL 029 AS OF 05/01/02                      
*PHASE T61B19A                                                                  
         TITLE 'RETAIL BILLING INPUT'                                           
T61B19   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,*BAT19*,R8,CLEAR=YES                                
         USING PROGD,RC                                                         
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING TWAD,RA                                                          
         SR    R6,R6               NO PROFILES                                  
         EJECT                                                                  
*              DIG OUT ADVERTISER VALUES                                        
         SPACE 2                                                                
         LA    R2,RETADVH                                                       
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVI   KEY+1,C'3'          RETAIL UNIT                                  
         MVC   KEY+2(1),RETADV                                                  
         MVC   KEY+3(6),=C'******' ADVERTISER DUMMY                             
         TM    4(R2),X'20'                                                      
         BO    RT2                                                              
         BAS   RE,GETACC                                                        
         MVC   RETADVN,ACCTNAME                                                 
         FOUT  RETADVNH                                                         
         OI    4(R2),X'20'                                                      
RT2      MVC   CONTROL,KEY         THIS IS THE ACCOUNT FOR                      
         MVC   CONTROLN,RETADVN    ALL POSTINGS IN THIS PROGRAM                 
         OC    CONTROLN,SPACES     ---                                          
         EJECT                                                                  
*              BUILD THE EXTRA BILLING ELEMENT                                  
         SPACE 2                                                                
         LA    R4,BILLEL                                                        
         USING TRXBILLD,R4                                                      
         MVC   TRXBEL(2),=X'4A36'  SET UP DEFAULTS                              
         MVC   TRXBMED(40),SPACES                                               
         ZAP   TRXBSHR,=P'10000'                                                
         BAS   RE,GETODAY                                                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,DATE)                                    
         MVC   TRXBDAT,DATE                                                     
         SPACE 2                                                                
         MVI   MEDSW,C'N'                                                       
         LA    R2,RETMEDH                                                       
         BAS   RE,ANY                                                           
         BAS   RE,MEDLOOK                                                       
         MVI   ERRNUM,2                                                         
         CLI   MEDSW,C'Y'                                                       
         BNE   ERROR                                                            
         MVC   TRXBMED,MEDNAME                                                  
         SPACE 1                                                                
         CLI   RETPROH+5,0                                                      
         BE    RT4                                                              
         SR    R1,R1                                                            
         IC    R1,RETPROH+5                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRXBPRD(0),RETPRO                                                
         SPACE 1                                                                
RT4      CLI   RETESTH+5,0                                                      
         BE    RT6                                                              
         IC    R1,RETESTH+5                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRXBEST(0),RETEST                                                
         SPACE 1                                                                
RT6      LA    R2,RETINVH                                                       
         BAS   RE,ANY                                                           
         IC    R1,RETINVH+5                                                     
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRXBINV(0),RETINV                                                
         SPACE 1                                                                
RT8      LA    R2,RETDATH                                                       
         BAS   RE,ANY                                                           
         MVI   ERRNUM,13                                                        
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,TRXBDAT)                                 
         MVC   DATE,TRXBDAT                                                     
         GOTO1 DATECHK,DMCB,DATE                                                
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
RT10     LA    R2,RETMOSH                                                       
         BAS   RE,ANY                                                           
         MVI   ERRNUM,13                                                        
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(9,TRXBMOS)                                 
         EJECT                                                                  
*              NOW VALIDATE/SAVE INPUT BILL DETAILS                             
         SPACE 2                                                                
RT20     LA    R2,RETFMKTH                                                      
         BAS   RE,ANY                                                           
         LA    R3,RETFCSHH                                                      
         BAS   RE,ANY                                                           
         ZAP   LINES,=P'11'                                                     
         LA    R5,BILLTAB                                                       
         USING BILLD,R5                                                         
         SPACE 1                                                                
RT22     MVC   KEY+3(12),SPACES                                                 
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
         CLC   8(8,R2),=C'NATIONAL'                                             
         BE    RT24                                                             
         CLC   8(5,R2),=C'TOTAL'                                                
         BNE   RT26                                                             
         ST    R3,TOTADDR                                                       
         SPACE 1                                                                
RT24     MVC   ACCTNUM,SPACES                                                   
         MVC   ACCTNAME,SPACES                                                  
         B     RT28                                                             
         SPACE 1                                                                
RT26     TM    4(R2),X'20'                                                      
         BZ    RT27                                                             
         ZIC   R3,0(R2)                                                         
         LA    R3,0(R2,R3)         R3 TO AMOUNT                                 
         ZIC   R4,0(R3)                                                         
         LA    R4,0(R3,R4)                                                      
         B     RT30                                                             
         SPACE 1                                                                
RT27     BAS   RE,GETACC                                                        
RT28     IC    RF,0(R3)                                                         
         LR    R4,R3               POINT TO NAME                                
         AR    R4,RF                                                            
         MVC   8(36,R4),ACCTNAME                                                
         FOUT  (R4)                                                             
         OI    4(R2),X'20'                                                      
         SPACE 1                                                                
RT30     SR    R7,R7                                                            
         MVC   BILLACC,KEY+3                                                    
         MVI   ERRNUM,25                                                        
         LR    R2,R3               NOW THE CASH                                 
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         IC    R7,5(R2)                                                         
         GOTO1 AMTVAL,DMCB,8(R2),(R7)                                           
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     RF,DMCB+4                                                        
         ZAP   BILLCASH,0(8,RF)                                                 
         SPACE 1                                                                
         LA    R5,L'BILLEN(R5)                                                  
         SR    RF,RF                                                            
         IC    RF,0(R4)                                                         
         LR    R2,R4                                                            
         AR    R2,RF               R2 TO NEXT MARKET                            
         IC    RF,0(R2)                                                         
         LR    R3,R2                                                            
         AR    R3,RF               R3 TO NEXT CASH                              
         SP    LINES,=P'1'                                                      
         CP    LINES,=P'0'         ANY MORE TO LOOK AT                          
         BNE   *+12                                                             
         MVI   0(R5),X'FF'                                                      
         B     RT48                                                             
         CLI   5(R2),0             ANY INPUT ON THIS ONE                        
         BNE   RT22                                                             
         SPACE 2                                                                
RT32     MVI   0(R5),X'FF'                                                      
         ZAP   DUB,LINES                                                        
         CVB   R5,DUB                                                           
RT34     SR    RF,RF                                                            
         IC    RF,0(R2)            POINT TO NAME                                
         AR    R2,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLC   8(36,R2),SPACES     CLEAR DOWN REST OF NAMES                     
         BE    RT36                                                             
         FOUT  (R2),SPACES,36                                                   
RT36     IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R5,RT34                                                          
         EJECT                                                                  
*              NOW BUILD SOME POSTING RECORDS                                   
         SPACE 2                                                                
RT48     LA    R5,BILLTAB                                                       
RT50     LA    R7,IOAREA+2                                                      
         USING DLDESCD,R7                                                       
         USING BILLD,R5                                                         
         LA    R4,BILLEL                                                        
         USING TRXBILLD,R4                                                      
         MVC   DLDSEL(2),=X'6414'                                               
         MVC   DLDSREF,TRXBINV                                                  
         MVC   DLDSDATE,DATE                                                    
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         MVI   DLDSNARR,C' '                                                    
         MVC   IOAREA(2),=X'00BE'  64+4A+69/6A (+3)                             
         SPACE 1                                                                
         ZAP   TRXBTOT,BILLCASH                                                 
         SR    RF,RF               NOW THE 4A ELEMENT                           
         IC    RF,DLDSLEN                                                       
         AR    R7,RF                                                            
         IC    RF,TRXBLEN                                                       
         BCTR  RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),TRXBEL                                                   
         SPACE 1                                                                
         LA    RF,1(RF)                                                         
         AR    R7,RF                                                            
         USING DLPOSTD,R7                                                       
         MVC   DLPSEL(2),=X'6971'                                               
         CLC   BILLACC(5),=C'TOTAL'                                             
         BNE   RT52                                                             
         MVI   YES,C'Y'                                                         
         BAS   RE,TOTCHK                                                        
         CLI   YES,C'Y'                                                         
         BE    RT51                                                             
         L     R2,TOTADDR                                                       
         MVI   ERRNUM,25                                                        
         B     ERROR                                                            
RT51     DS    0H                                                               
         MVI   DLPSEL,X'6A'        CREDIT FOR TOTAL,ELSE DEBIT                  
         MVC   DLPSCRAC,CONTROL                                                 
         MVC   DLPSCRNM,CONTROLN                                                
         MVC   DLPSDBAC,SPACES                                                  
         MVC   DLPSDBAC+1(2),MEDIA                                              
         MVC   DLPSDBNM,SPACES                                                  
         MVC   DLPSDBNM(15),MEDNAME                                             
         B     RT54                                                             
         SPACE 1                                                                
RT52     MVC   DLPSDBAC,CONTROL                                                 
         MVC   DLPSDBNM,CONTROLN                                                
         MVC   DLPSCRAC,SPACES                                                  
         MVC   DLPSCRAC+1(2),MEDIA                                              
         CLC   BILLACC(8),=C'NATIONAL'                                          
         BE    *+10                                                             
         MVC   DLPSCRAC+3(12),BILLACC                                           
         MVC   DLPSCRNM,SPACES                                                  
         MVC   DLPSCRNM(15),MEDNAME                                             
         SPACE 1                                                                
RT54     MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,BILLCASH                                                
         MVC   DLPSANAL(3),=X'404000'   END BYTE AS WELL                        
         SPACE 1                                                                
         BAS   RE,PUTDAY                                                        
         XC    WORK,WORK                                                        
         MVC   WORK(6),TRXBINV                                                  
         L     RF,DMCB+8                                                        
         MVC   WORK+10(4),0(RF)    DISK ADDRESS                                 
         ZAP   TRANSAMT,BILLCASH                                                
         BAS   RE,ADTWA1                                                        
         SPACE 2                                                                
         LA    R5,L'BILLEN(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   RT50                                                             
         SPACE 1                                                                
         LA    R2,RETMEDH                                                       
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*              LOOK UP THE MEDIA TABLE                                          
         SPACE 2                                                                
MEDLOOK  NTR1                                                                   
         LA    R4,MEDIATAB                                                      
MD2      CLI   0(R4),X'FF'                                                      
         BE    MDEXIT                                                           
         CLC   0(2,R4),8(R2)                                                    
         BE    MDYES                                                            
         LA    R4,17(R4)                                                        
         B     MD2                                                              
         SPACE 1                                                                
MDYES    MVI   MEDSW,C'Y'                                                       
         MVC   MEDIA,8(R2)                                                      
         MVC   MEDNAME,2(R4)                                                    
MDEXIT   XIT1                                                                   
         SPACE 3                                                                
MEDIATAB DC    C'ST',CL15'SPOT TELEVISION'                                      
         DC    C'SR',CL15'SPOT RADIO'                                           
         DC    C'SN',CL15'NETWORK TV'                                           
         DC    C'SX',CL15'NETWORK RADIO'                                        
         DC    C'PN',CL15'NEWSPAPERS'                                           
         DC    C'PM',CL15'MAGAZINES'                                            
         DC    C'PS',CL15'SUPPLEMENTS'                                          
         DC    C'PT',CL15'TRADE'                                                
         DC    C'PO',CL15'OUTDOOR'                                              
         DC    C'PR',CL15'PRODUCTION'                                           
         DC    C'XX',CL15'MISCELLANEOUS'                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
MAXBTREC DC    H'50'                                                            
         EJECT                                                                  
*              CHECK TOTAL OF BILLS TO INPUT AMOUNT                             
         SPACE 2                                                                
TOTCHK   CP    BILLCASH,CSLSTCUR+LSTBCSHA-LSTTABD(L'LSTBCSHA)                   
         BE    *+8                                                              
         MVI   YES,C'N'                                                         
         BR    RE                                                               
         SPACE 2                                                                
*OTCHK   NTR1                                                                   
*        LH    R4,MAXBTREC                                                      
*        L     R3,ATWA1                                                         
*        LA    R3,8(R3)                                                         
*        USING ENTRYD,R3                                                        
*        ZAP   DUB,=P'0'                                                        
*OTC2    OC    0(ENTRYLEN,R3),0(R3)                                             
*        BZ    TOTC4                                                            
*        AP    DUB,ENTRYAMT                                                     
*        LA    R3,ENTRYLEN(R3)                                                  
*        BCT   R4,TOTC2                                                         
*        SPACE 1                                                                
*OTC4    CP    BILLCASH,DUB                                                     
*        BE    TOTC6                                                            
*        MVI   YES,C'N'                                                         
*        B     TOTEXT                                                           
*        SPACE 1                                                                
*OTC6    LH    R6,MAXBTREC         ITEM COUNT                                   
*        SR    R6,R4                                                            
*        LA    R6,1(R6)                                                         
*        EDIT  (R6),(2,CONIC),ALIGN=LEFT                                        
*        STC   R0,CONICH+5                                                      
*        STC   R0,CONICH+7                                                      
*        OI    CONICH+4,X'80'                                                   
*        FOUT  CONICH                                                           
*        ZAP   DUB,BILLCASH                                                     
*        CVB   R7,DUB                                                           
*        MH    R7,=H'2'                                                         
*        EDIT  (R7),(12,CONBT),2,FLOAT=-,ALIGN=LEFT                             
*        STC   R0,CONBTH+5                                                      
*        STC   R0,CONBTH+7                                                      
*        FOUT  CONBTH                                                           
*OTEXT   XIT1                                                                   
*        DROP  R3                                                               
         EJECT                                                                  
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACBATDSECT                                                     
       ++INCLUDE ACBATE5D                                                       
         ORG   TWAHOLE                                                          
YES      DS    CL1                                                              
TOTADDR  DS    F                                                                
CONTROL  DS    CL15                                                             
CONTROLN DS    CL36                                                             
DATE     DS    CL3                                                              
MEDSW    DS    CL1                                                              
MEDIA    DS    CL2                                                              
MEDNAME  DS    CL15                                                             
BILLTAB  DS    CL199               11 X 18 (+ 1)                                
LINES    DS    CL2                                                              
BILLEL   DS    CL60                FOR '4A' ELEMENT                             
*                                                                               
         EJECT                                                                  
*                                                                               
PROGD    DSECT                                                                  
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         SPACE 2                                                                
*              DSECT FOR A LINE IN THE BILL TABLE                               
         SPACE 2                                                                
BILLD    DSECT                                                                  
BILLEN   DS    0CL18                                                            
BILLACC  DS    CL12                                                             
BILLCASH DS    PL6                                                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENDAY                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACBAT19   05/01/02'                                      
         END                                                                    
