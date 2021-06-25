*          DATA SET TAXTRAFT   AT LEVEL 009 AS OF 10/11/10                      
*PHASE TAXTRAFA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
         TITLE 'TAXTRAFT - TALENT EXTRACT POST PROCESSOR'                       
********************************************************************            
* THIS MODULE READS THE 6 TALENT EXTRACT OUTPUT DATASETS TO        *            
* GET THE DATES FOR WHICH THE EXTRACT WAS RUN AND USE THAT TO      *            
* DYNAMICALLY ALLOCATE DATASETS WITH THE DATES IN THE DSN.         *            
* THE SECOND RECORD OF EACH FILE SHOULD BE A 05399 RUN DATES REC   *            
*                                                                  *            
* THEN IT CALLS DFSORT TO CREATE THE OUTPUT DATASETS               *            
*                                                                  *            
* THEN IT SENDS AN MQ MESSAGE FOR EACH DSN (KEEP ON LAUGHING)      *            
* TO SEE THE MQ MESSAGE USE EJES TO VIEW EDISKED(T) ON SY1         *            
********************************************************************            
                                                                                
TAXTRAFT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*XTRAFT*,=V(REGSAVE)                                           
*                                                                               
AFT2     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    AFT10                                                            
         CLC   =C'RUN=TEST',CARD                                                
         BE    AFT4                                                             
         CLC   =C'RUN=PROD',CARD                                                
         BE    AFT4                                                             
         CLC   =C'WRITE=NO',CARD                                                
         BE    AFT8                                                             
         CLC   =C'DDSIO=',CARD                                                  
         BE    AFT6                                                             
         DC    H'0'                                                             
*                                                                               
AFT4     MVC   RUNTYPE,CARD+4                                                   
         B     AFT2                                                             
*                                                                               
AFT6     L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),CARD+6                                                   
         B     AFT2                                                             
*                                                                               
AFT8     MVI   OPTWRITE,C'N'                                                    
         B     AFT2                                                             
*                                                                               
AFT10    L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         ZAP   MAXLINE,=P'58'      SET MAXLINES                                 
*                                                                               
         LA    R4,INPFLST                                                       
*                                                                               
AFT12    MVC   FILEIN+40(8),0(R4)  MOVE FILENAME TO DCB                         
*                                                                               
         OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GET   FILEIN,RECLEN       READ FIRST RECORD                            
         GET   FILEIN,RECLEN       REC 2 SHOULD BE RUN PERIOD                   
*                                                                               
         CLC   =C'05399',REC                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* GET RID OF FUNNY DATES                                                        
                                                                                
         GOTO1 =V(DATCON),DMCB,REC+6,(X'20',WORK)                               
         GOTO1 (RF),(R1),REC+12,(X'20',WORK+6)                                  
*                                                                               
         CLI   SVDATES,0                                                        
         BNE   *+10                                                             
         MVC   SVDATES,WORK        MOVE DATES FIRST TIME                        
*                                                                               
         CLC   SVDATES,WORK                                                     
         BE    *+6                                                              
         DC    H'0'                DIE IF DATES DO NOT AGREE                    
*                                                                               
AFT14    CLOSE FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,8(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   AFT12                                                            
         EJECT                                                                  
*==================================================================             
* NOW DYNAMICALLY ALLOCATE OUTPUT FILE DSNS WITH DATES                          
* TWO VERY SIMILAR FILENAMES, 1 FOR DATA, 1 FOR TOTAL RECORD                    
* DSN LOOKS LIKE SFTPDISK.PROD.TAXCML.SMMDDYY.EMMDDYY                           
*==================================================================             
                                                                                
         LA    R4,OUTFLST                                                       
*                                                                               
AFT20    MVC   FILEOUT+40(8),0(R4) MOVE FILENAME TO DCB                         
*                                                                               
         BAS   RE,GENDSN                                                        
*                                                                               
         L     RF,=V(DYNALLOC)     TRY ALLOCATING NEW DATASET                   
         GOTO1 (RF),DMCB,(X'80',(R4)),(X'85',ALLOCS),(X'40',MYDSN),0            
         OC    12(4,R1),12(R1)     ANY DYNALLOC ERROR?                          
         BZ    AFT22               NO                                           
*                                  TRY DISP=SHR, BUT NO FREE=CLOSE              
         GOTO1 (RF),(R1),(X'FF',(R4)),(X'85',MYDSN)                             
*                                                                               
AFT22    DS    0H                                                               
         LA    R4,L'OUTFLST(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   AFT20                                                            
*                                                                               
         SR    R1,R1               USE TOOLIN INTERFACE                         
         LINK  EP=ICETOOL          CALL ICETOOL                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                AT LEAST ONE BAD ICETOOL OPERATOR            
         EJECT                                                                  
*===============================================================                
* GENERATE MQ MESSAGE TO SEND EACH DATASET                                      
*===============================================================                
                                                                                
         CLI   OPTWRITE,C'N'                                                    
         BE    AFTX                                                             
*                                                                               
         LA    R4,OUTFLST                                                       
                                                                                
AFT30    GOTO1 =V(DYNALLOC),DMCB,(C'U',(R4))  UNALLOCATE DATASET                
*                                                                               
         MVI   DMCB+8,X'A0'        SUPPRESS LENGTH FOR MESSAGE & HDR            
         CLI   RUNTYPE,C'T'        IS THIS A TEST RUN?                          
         BNE   *+8                 NO                                           
         OI    DMCB+8,X'01'        YES -PUT TO TEST MQ BROKER                   
                                                                                
         GOTO1 =V(MQRPT),DMCB,(0,=C'OPEN'),(0,MQFILEID),,0                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         BRAS  RE,GENDSN            SET MVS DSN                                 
         MVC   MQFILE(48),MYDSN+14                                              
                                                                                
* NOW SET FILE NAME OF FILE TO BE SENT BY MQ (NO HLQ)                           
                                                                                
         MVC   MQDSN,SPACES                                                     
         MVC   MQDSN(48),8(R4)      MOVE DSN BUT NOT HLQ                        
*                                                                               
         MVC   MQSDATE(4),SVDATES+2   MOVE START MMYY                           
         MVC   MQSDATE+4(2),SVDATES                                             
         MVC   MQEDATE(4),SVDATES+8                                             
         MVC   MQEDATE+4(2),SVDATES                                             
*                                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'PUT'),MQMSG,MQMSGLNQ,0                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(MQRPT),DMCB,(0,=C'CLOSE'),0,0,0                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,L'OUTFLST(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   AFT30                                                            
*                                                                               
AFTX     XBASE                                                                  
MQFILEID DC    CL16'EDIHUBFT********'                                           
         EJECT                                                                  
*===============================================================                
* GENERATE DSN FOR FILENAME AT 0(R4)                                            
* SINCE THESE ARE MVS DSN'S, SFTPDISK.PROD/TEST MUST BE THERE                   
*===============================================================                
                                                                                
GENDSN   DS    0H                                                               
         MVC   MYDSN,SPACES                                                     
         MVC   MYDSN(14),=C'SFTPDISK.PROD.'                                     
         MVC   MYDSN+9(4),RUNTYPE  SET TEST/PROD                                
*                                                                               
         LA    R5,MYDSN+14         POINT BEYOND HLQ                             
         MVC   0(8,R5),0(R4)       SET DDNAME                                   
*                                                                               
         LA    R5,9(R5)            POINT BEYOND DDNAME                          
         CLI   0(R5),C' '          FIND LAST CHAR                               
         BH    *+8                                                              
         BCT   R5,*-8                                                           
*                                                                               
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'.'                                                       
         MVI   1(R5),C'S'          SET ALPHA FOR START DATE                     
         MVC   2(4,R5),SVDATES+2   MOVE MMDD                                    
         MVC   6(2,R5),SVDATES     MOVE YY                                      
*                                                                               
         MVI   8(R5),C'.'                                                       
         MVI   9(R5),C'E'          SET ALPHA FOR END DATE                       
         MVC   10(4,R5),SVDATES+8  MOVE MMDD                                    
         MVC   14(2,R5),SVDATES+6  MOVE YY                                      
         BR    RE                                                               
*                                                                               
INPFLST  DS    0XL8                                                             
         DC    CL8'TALCML  '                                                    
         DC    CL8'TALRES  '                                                    
         DC    CL8'TALSES  '                                                    
         DC    CL8'TALFNL  '                                                    
         DC    CL8'TALUSE  '                                                    
         DC    CL8'TALREF  '                                                    
INPFLSTX EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
MYDSN    DS    CL64                                                             
MYDDNAME EQU   MYDSN+14,8                                                       
ALLOCS   DC    AL3(20),AL3(20)     PRIMARY 20 TRKS/2ND 10 TRKS                  
*                                                                               
* NOTE: THE 'ROUTE' IS PASSED IN THE MQOPEN AS THE HEADER                       
*                                                                               
MQMSG    DS    0D                                                               
MQHID    DC    CL6'DANOT1'         HUB RECORD ID                                
MQSYS    DC    C'TAL'              SYSTEM=TAL                                   
MQAGYID  DC    C'PPCH'             AGENCY ID (FROM ACCESS REC FOR DS)           
MQQUAL   DC    CL16'BILLING'       QUALIFIER                                    
MQDATE   DS    CL6                 DATE                                         
MQTIME   DS    CL6                 TIME HHMMSS                                  
         ORG   MQDATE              REDEFINE FOR TAX                             
MQSDATE  DS    CL6                                                              
MQEDATE  DS    CL6                                                              
         ORG                                                                    
MQDATA1  DS    CL32                                                             
MQDATA2  DS    CL32                                                             
         ORG   MQDATA1                                                          
MQDSN    DS    CL64            DSN FOR FILE TO BE SENT BY MQ (NO HLQ)           
         ORG                                                                    
MQFILE   DS    CL64            DSN OF CORRESPONDING MVS FILE                    
MQMSGLNQ EQU   *-MQMSG                                                          
*                                                                               
DMCB     DS    6A                                                               
         ORG   *-24                                                             
DM1      DS    A                                                                
DM2      DS    A                                                                
DM3      DS    A                                                                
DM4      DS    A                                                                
DM5      DS    A                                                                
DM6      DS    A                                                                
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
CARD     DS    CL80                                                             
RUNTYPE  DS    CL4                                                              
SVDATES  DS    CL12                                                             
WORK     DS    XL32                                                             
OPTWRITE DS    CL1                                                              
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,LRECL=512,MACRF=GM,     X        
               EODAD=AFT14                                                      
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=512,MACRF=PM              
         EJECT                                                                  
OUTFLST  DS    0CL56                                                            
         DC    CL8'TAXAGY  '                                                    
         DC    CL48'REFERENCE_AGENCY_TP'                                        
         DC    CL8'TAXAGYT '                                                    
         DC    CL48'REFERENCE_AGENCY_CONTROL_TP'                                
*                                                                               
         DC    CL8'TAXPRD  '                                                    
         DC    CL48'REFERENCE_PRODUCT_TP'                                       
         DC    CL8'TAXPRDT '                                                    
         DC    CL48'REFERENCE_PRODUCT_CONTROL_TP'                               
*                                                                               
         DC    CL8'TAXNET  '                                                    
         DC    CL48'REFERENCE_NETWORK_TP'                                       
         DC    CL8'TAXNETT '                                                    
         DC    CL48'REFERENCE_NETWORK_CONTROL_TP'                               
*                                                                               
         DC    CL8'TAXUTY  '                                                    
         DC    CL48'REFERENCE_USECATEGORY_TP'                                   
         DC    CL8'TAXUTYT '                                                    
         DC    CL48'REFERENCE_USECATEGORY_CONTROL_TP'                           
*                                                                               
         DC    CL8'TAXMDC  '                                                    
         DC    CL48'REFERENCE_MEDIACATEGORY_TP'                                 
         DC    CL8'TAXMDCT '                                                    
         DC    CL48'REFERENCE_MEDIACATEGORY_CONTROL_TP'                         
*                                                                               
         DC    CL8'TAXCMT  '                                                    
         DC    CL48'REFERENCE_COMMTYPE_TP'                                      
         DC    CL8'TAXCMTT '                                                    
         DC    CL48'REFERENCE_COMMTYPE_CONTROL_TP'                              
*                                                                               
         DC    CL8'TAXPRF  '                                                    
         DC    CL48'REFERENCE_PERFORMER_TP'                                     
         DC    CL8'TAXPRFT '                                                    
         DC    CL48'REFERENCE_PERFORMER_CONTROL_TP'                             
*                                                                               
         DC    CL8'TAXCTG  '                                                    
         DC    CL48'REFERENCE_CATEGORY_TP'                                      
         DC    CL8'TAXCTGT '                                                    
         DC    CL48'REFERENCE_CATEGORY_CONTROL_TP'                              
*                                                                               
         DC    CL8'TAXADV  '                                                    
         DC    CL48'REFERENCE_ADVERTISER_TP'                                    
         DC    CL8'TAXADVT '                                                    
         DC    CL48'REFERENCE_ADVERTISER_CONTROL_TP'                            
*                                                                               
         DC    CL8'TAXOVR  '                                                    
         DC    CL48'OS_PCTG_AMOUNT_DETAIL_TP'                                   
         DC    CL8'TAXOVRT '                                                    
         DC    CL48'OS_PCTG_AMOUNT_DETAIL_CONTROL_TP'                           
*                                                                               
         DC    CL8'TAXUSE  '                                                    
         DC    CL48'TALENT_PAYMENT_USE_AUTHORIZATION_TP'                        
         DC    CL8'TAXUSET '                                                    
         DC    CL48'TALENT_PAYMENT_USE_AUTHORIZATION_CONTROL_TP'                
*                                                                               
         DC    CL8'TAXFNL  '                                                    
         DC    CL48'FINAL_CAST_COMPLETION_REPORT_TP'                            
         DC    CL8'TAXFNLT '                                                    
         DC    CL48'FINAL_CAST_COMPLETION_REPORT_CONTROL_TP'                    
*                                                                               
         DC    CL8'TAXCML  '                                                    
         DC    CL48'PERFORMER_SESSION_DATA_TP'                                  
         DC    CL8'TAXCMLT '                                                    
         DC    CL48'PERFORMER_SESSION_DATA_CONTROL_TP'                          
*                                                                               
         DC    CL8'TAXRES  '                                                    
         DC    CL48'RESIDUAL_AMOUNT_TP'                                         
         DC    CL8'TAXREST '                                                    
         DC    CL48'RESIDUAL_AMOUNT_CONTROL_TP'                                 
*                                                                               
         DC    CL8'TAXSES  '                                                    
         DC    CL48'SESSION_HOLDING_TP'                                         
         DC    CL8'TAXSEST '                                                    
         DC    CL48'SESSION_HOLDING__CONTROL'                                   
OUTFLSTX DC    X'FF'                                                            
*                                                                               
RECLEN   DS    F                                                                
REC      DS    XL1024                                                           
                                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009TAXTRAFT  10/11/10'                                      
         END                                                                    
