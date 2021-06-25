*          DATA SET ACNV04     AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACNV04A                                                                  
         TITLE '- CONVERSION PROGRAM / RECORD COUNT '                           
**********************************************************************          
* PARAMS VIA R1                                                      *          
* XL1   COUNT/PRINT  X'00'=INPUT                                     *          
*                    X'01'=DELETE                                    *          
*                    X'02'=ADD                                       *          
*                    X'03'=OUTPUT                                    *          
*                    X'FF'=PRINT                                     *          
* AL3   A(RECORD)                                                    *          
**********************************************************************          
         SPACE 1                                                                
ACNV04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CNVT                                                           
         USING ACNVD,R9                                                         
         MVC   PARMS,0(R1)                                                      
*                                                                               
         CLI   PCODE,X'FF'                                                      
         BNE   DCCOUNT                                                          
         B     DCPRINT                                                          
         EJECT                                                                  
**********************************************************************          
* COUNT ALL RECORDS BY TYPE AND STATUS FOR EACH COMPANY              *          
**********************************************************************          
         SPACE 1                                                                
DCCOUNT  XR    R2,R2                                                            
         ICM   R2,7,PAREC          A(RECORD)                                    
         USING TRNRECD,R2                                                       
         GOTO1 ACRECTYP,DMCB,(C'D',(R2))                                        
         MVC   WRECTYPE,0(R1)      RECORD TYPE                                  
         MVC   WCPYCODE,1(R1)      COMPANY CODE                                 
*                                                                               
         L     R3,ARECT            FIND ENTRY FOR RECORD TYPE                   
         USING RECTABD,R3                                                       
         XR    R4,R4               R4=DISPLACEMENT INTO COUNTER TABLE           
*                                                                               
DCNT02   CLI   RECTABD,EOT                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RECTYPE,WRECTYPE    MATCH ON RECORD TYPE                         
         BE    DCNT10                                                           
         LA    R4,CNTL(R4)                                                      
         LA    R3,RECTABL(R3)                                                   
         B     DCNT02                                                           
*                                                                               
DCNT10   TM    RECINDS1,RECIFILE   TEST FOR FILE TOTALS ONLY                    
         BZ    *+12                                                             
         LA    R4,FILECNTS(R4)     R4=A(COUNTERS FOR FILE)                      
         B     DCNT12                                                           
*                                                                               
         XR    R1,R1                                                            
         IC    R1,WCPYCODE                                                      
         MH    R1,=Y(CPYTABL)                                                   
         A     R1,ACPYTAB                                                       
         USING CPYTABD,R1                                                       
         MVC   CPYCODE,WCPYCODE                                                 
         OI    CPYINDS,CPYICNT                                                  
         LA    R4,CPYCNTS(R4)      R4=A(COUNTERS FOR COMPANY)                   
         DROP  R1                                                               
*                                                                               
         USING CNTD,R4             R4=A(COUNTERS)                               
DCNT12   SR    R1,R1               INCREMENT COUNTER                            
         IC    R1,PCODE                                                         
         SLL   R1,2                                                             
         LA    R1,CNTINP(R1)                                                    
         ICM   RE,15,0(R1)                                                      
         AH    RE,HONE                                                          
         STCM  RE,15,0(R1)                                                      
         B     XIT                                                              
         DROP  R4,R3,R2                                                         
         EJECT                                                                  
**********************************************************************          
* PRINT COMPANY TOTALS BY RECTYPE/STATUS FROM ACCUMBUF,              *          
* ACCUMULATE AND PRINT RECTYPE/STATUS FILE TOATLS                    *          
**********************************************************************          
         SPACE 1                                                                
DCPRINT  L     R8,CPRINT                                                        
         USING DPRINT,R8                                                        
         ZAP   MAXLINE,=P'57'                                                   
         L     R6,BOXAREA                                                       
         USING BOXD,R6                                                          
         BAS   RE,SETPRINT                                                      
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXINIT,0                                                        
         MVC   BOXCOLS,SPACES                                                   
         LA    R7,BOXCOLS                                                       
         USING PBXLD,R7                                                         
         MVI   PBXL,C'L'                                                        
         MVI   PBXC1,C'C'                                                       
         MVI   PBXC2,C'C'                                                       
         MVI   PBXC3,C'C'                                                       
         MVI   PBXC4,C'C'                                                       
         MVI   PBXR,C'R'                                                        
*                                                                               
         MVC   TITLE+((L'TITLE-L'T@TITLE)/2)(L'T@TITLE),T@TITLE                 
         MVI   MID1,0              FORCE BLANK LINES                            
         MVI   MID3,0                                                           
         MVI   MID4,0                                                           
         LA    R7,MID2                                                          
         MVC   MCPY,T@CPY                                                       
         MVI   SUB1,0                                                           
         LA    R7,SUB2                                                          
         MVC   PREC,T@REC                                                       
         MVC   PINP,T@INP                                                       
         MVC   PPUR,T@PUR                                                       
         MVC   PADD,TADD                                                        
         MVC   POUT,T@OUT                                                       
         L     R4,ACPYTAB                                                       
         USING CPYTABD,R4          R4=A(COMPANY TABLE)                          
         LA    R3,CPYTABN                                                       
*                                                                               
DCPRT02  TM    CPYINDS,CPYICNT     TEST ANYTHING FOR COMPANY                    
         BZ    DCPRT08                                                          
         LA    R7,MID2                                                          
         GOTO1 HEXOUT,DMCB,CPYCODE,MCODE,L'CPYCODE,=C'TOG'                      
         GOTO1 PRNTCPY,CPYCNTS                                                  
*                                                                               
DCPRT08  LA    R4,CPYTABL(R4)                                                   
         BCT   R3,DCPRT02                                                       
         LA    R2,MID2                                                          
         MVC   MFTOTS,T@FTOTS                                                   
         GOTO1 PRNTCPY,FILECNTS                                                 
         MVI   BOXYORN,C'N'                                                     
         BAS   RE,SETPRINT                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT TOTALS FOR COMPANY                                 *         
*                                                                     *         
* NTRY: R1=A(COUNTERS TABLE)                                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTCPY  NTR1  ,                                                                
         LR    R3,R1               R3=A(COUNTRES)                               
         USING CNTD,R3                                                          
         L     R2,ARECT                                                         
         USING RECTABD,R2          R2=A(RECORD TYPE TABLE)                      
         LA    R4,FILECNTS         R4=A(FILE TOTAL COUNTERS)                    
*                                                                               
PCPY02   OC    CNTD(CNTL),CNTD                                                  
         BZ    PCPY08              TEST ANY COUNTED                             
         LA    R7,P                                                             
         MVC   PREC(L'RECNAME),RECNAME  OUTPUT ROW                              
         GOTO1 PRNTROW,DMCB,CNTD,RECTABD                                        
         GOTO1 ADDCNT,DMCB,(R4),CNTD                                            
*                                                                               
PCPY08   LA    R3,CNTL(R3)                                                      
         LA    R4,CNTL(R4)                                                      
         LA    R2,RECTABL(R2)                                                   
         CLI   RECTABD,EOT                                                      
         BNE   PCPY02                                                           
         DROP  R2,R3                                                            
*                                                                               
         MVC   HALF,LINE           DRAW HORIZONTAL LINE                         
         AP    HALF,=P'1'          UNLESS AT BOTTOM OF PAGE                     
         CLC   HALF,MAXLINE                                                     
         BH    PCPY10                                                           
         BE    *+8                                                              
         MVI   BOXREQ,C'B'                                                      
         GOTO1 PRINTER                                                          
*                                                                               
PCPY10   LA    R2,TOT              PRINT TOTALS                                 
         USING TOTD,R2                                                          
PCPY12   OC    TOTCNT,TOTCNT                                                    
         BZ    PCPY18                                                           
         MVC   PREC,TOTNAME                                                     
         GOTO1 PRNTROW,DMCB,TOTCNT,0                                            
         XC    TOTCNT,TOTCNT                                                    
*                                                                               
PCPY18   LA    R2,TOTL(R2)                                                      
         CLI   TOTD,EOT                                                         
         BNE   PCPY12                                                           
         DROP  R2                                                               
*                                                                               
         CLC   LINE,MAXLINE                                                     
         BNL   PCPY20                                                           
         MVI   BOXREQ,C'C'         CLOSE BOX                                    
         GOTO1 PRINTER                                                          
*                                                                               
PCPY20   ZAP   LINE,=P'99'         SET NEW PAGE                                 
         ZAP   PAGE,=P'1'                                                       
         B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
*  PRINT TOTALS FOR A ROW                                             *         
*                                                                     *         
* NTRY: P1=A(COUNTER TABLE ENTRY)                                     *         
*       P2=A(FILE TOTAL TABLE ENTRY OR ZERO)                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTROW  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING CNTD,R2             R2=COUNTERS TABLE                            
         USING RECTABD,R3          R3=RECORD TABLE ENTRY                        
         LA    R7,P                                                             
*                                                                               
         LTR   R3,R3                                                            
         BZ    PROW10                                                           
         LA    R4,TOT                                                           
         USING TOTD,R4                                                          
PROW02   MVC   BYTE,TOTTYPE                                                     
         NC    BYTE,RECINDS1                                                    
         BZ    PROW08                                                           
         GOTO1 ADDCNT,DMCB,TOTCNT,CNTD                                          
PROW08   LA    R4,TOTL(R4)                                                      
         CLI   TOTD,EOT                                                         
         BNE   PROW02                                                           
         DROP  R4                                                               
*                                                                               
PROW10   ICM   R4,15,CNTINP        RECORDS INPUT                                
         BZ    PROW12                                                           
         EDIT  (R4),(10,PINP)                                                   
*                                                                               
PROW12   ICM   R4,15,CNTPUR        RECORDS PURGED                               
         BZ    PROW13                                                           
         EDIT  (R4),(10,PPUR)                                                   
*                                                                               
PROW13   ICM   R4,15,CNTADD        RECORDS PURGED                               
         BZ    PROW14                                                           
         EDIT  (R4),(10,PADD)                                                   
*                                                                               
PROW14   ICM   R4,15,CNTPUT        RECORDS OUTPUT                               
         BZ    PROW16                                                           
         EDIT  (R4),(10,POUT)                                                   
*                                                                               
PROW16   GOTO1 PRINTER                                                          
*                                                                               
XIT      XIT1  ,                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD COUNTERS                                             *         
*                                                                     *         
* NTRY: P1=A(COUNTERS TO BE ADDED TO)                                 *         
*       P2=A(COUNTERS TO BE ADDED)                                    *         
***********************************************************************         
         SPACE 1                                                                
ADDCNT   STM   RE,R1,12(RD)                                                     
         LM    RE,RF,0(R1)                                                      
         USING CNTD,RE                                                          
         ICM   R0,15,CNTINP-CNTD(RF)                                            
         BZ    ADDCNT2                                                          
         ICM   R1,15,CNTINP                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTINP                                                     
*                                                                               
ADDCNT2  ICM   R0,15,CNTPUR-CNTD(RF)                                            
         BZ    ADDCNT3                                                          
         ICM   R1,15,CNTPUR                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTPUR                                                     
*                                                                               
ADDCNT3  ICM   R0,15,CNTADD-CNTD(RF)                                            
         BZ    ADDCNT4                                                          
         ICM   R1,15,CNTADD                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTADD                                                     
*                                                                               
ADDCNT4  ICM   R0,15,CNTPUT-CNTD(RF)                                            
         BZ    ADDCNTX                                                          
         ICM   R1,15,CNTPUT                                                     
         AR    R1,R0                                                            
         STCM  R1,15,CNTPUT                                                     
         DROP  RE                                                               
ADDCNTX  LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE PRINT DSECT                                   *         
***********************************************************************         
         SPACE 1                                                                
SETPRINT MVC   TITLE,SPACES                                                     
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
         MVC   SUB1,SPACES                                                      
         MVC   SUB2,SPACES                                                      
         MVC   SUB3,SPACES                                                      
         MVC   P,SPACES                                                         
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
ACPYTAB  DC    A(CPYTAB)                                                        
         SPACE 1                                                                
HONE     DC    H'1'                                                             
         DS    0F                                                               
PARMS    DS    0XL4                                                             
PCODE    DS    XL1                 INPUT CODE                                   
PAREC    DS    AL3                 A(RECORD)                                    
*                                                                               
WRECTYPE DS    XL1                                                              
WCPYCODE DS    XL1                                                              
         SPACE 1                                                                
T@TITLE  DC    C'Logical Record Counts'                                         
T@REC    DC    CL(L'PREC)'Record Type'                                          
T@INP    DC    CL(L'PINP)'     Input'                                           
T@PUR    DC    CL(L'PPUR)'    Purged'                                           
TADD     DC    CL(L'PADD)'     Added'                                           
T@OUT    DC    CL(L'POUT)'    Output'                                           
T@FTOTS  DC    CL(L'MFTOTS)'File Totals'                                        
T@CPY    DC    CL(L'MCPY)'Company'                                              
T@UNKEQU DC    CL(L'RECNAME)'''000'' Unknown record equate'                     
T@UNKTYP DC    CL(L'RECNAME)'X''00'' Unknown record type'                       
T@UNKOTH DC    CL(L'RECNAME)'.......All other unknowns'                         
         EJECT                                                                  
         LTORG                                                                  
RECTABN  EQU   150                                                              
FILECNTS DC    (RECTABN)XL(CNTL)'00' FILE TOTAL COUNTERS                        
TOT      DS    0XL(TOTL)                                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of Unknowns'                               
         DC    AL1(RECIUNK),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCARC records'                         
         DC    AL1(RECIARC),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCMST records'                         
         DC    AL1(RECIMST),XL(CNTL)'00'                                        
*                                                                               
         DC    CL(L'TOTNAME)'* Total of Records on File'                        
         DC    AL1(RECIMST+RECIARC),XL(CNTL)'00'                                
*                                                                               
         DC    CL(L'TOTNAME)'* Total of ACCDIR records'                         
         DC    AL1(RECIDIR),XL(CNTL)'00'                                        
*                                                                               
TOTX     DC    AL1(EOT)                                                         
         EJECT                                                                  
CPYTAB   DC    (256*CPYTABL)X'00'                                               
         DS    0X                                                               
CPYTABN  EQU   (*-CPYTAB)/CPYTABL                                               
         EJECT                                                                  
***********************************************************************         
* TOTALS TABLE (FOR COMPANY)                                          *         
***********************************************************************         
         SPACE 1                                                                
TOTD     DSECT                                                                  
TOTNAME  DS    CL(L'RECNAME)       TOTAL NAME                                   
TOTTYPE  DS    XL1                 TOTAL TYPE (SEE RECINDS1)                    
TOTCNT   DS    XL(CNTL)            COUNTERS                                     
TOTL     EQU   *-TOTD                                                           
         SPACE 2                                                                
***********************************************************************         
* COUNTER TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CNTD     DSECT                                                                  
CNTINP   DS    XL4                 INPUT                                        
CNTPUR   DS    XL4                 PURGE                                        
CNTADD   DS    XL4                 PURGE                                        
CNTPUT   DS    XL4                 OUTPUT                                       
CNTL     EQU   *-CNTD                                                           
         EJECT                                                                  
***********************************************************************         
* COMPANY TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
CPYTABD  DSECT                                                                  
CPYCODE  DS    XL1                 COMPANY CODE                                 
CPYINDS  DS    XL1                 INDICATORS                                   
CPYICNT  EQU   X'80'               RECORD COUNTED FOR THIS COMPANY              
CPYCNTS  DS    (RECTABN)XL(CNTL)   COUNTERS FOR RECORD TYPES                    
CPYTABL  EQU   *-CPYTABD                                                        
         SPACE 2                                                                
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PBXLD    DSECT                                                                  
         DS    CL((132-PBXLEN)/2)                                               
PBXL     DS    CL1                                                              
         DS    CL2                                                              
PREC     DS    CL(L'RECNAME)                                                    
         DS    CL1                                                              
PBXC1    DS    CL1                                                              
         DS    CL3                                                              
PINP     DS    CL10                                                             
         DS    CL3                                                              
PBXC2    DS    CL1                                                              
         DS    CL3                                                              
PPUR     DS    CL10                                                             
         DS    CL3                                                              
PBXC3    DS    CL1                                                              
         DS    CL3                                                              
PADD     DS    CL10                                                             
         DS    CL3                                                              
PBXC4    DS    CL1                                                              
         DS    CL3                                                              
POUT     DS    CL10                                                             
         DS    CL3                                                              
PBXR     DS    CL1                                                              
PBXLEN   EQU   *-PBXL                                                           
         ORG   PBXLD+(TITLE-HEAD1+((L'TITLE-L'MFTOTS)/2))                       
MFTOTS   DS    0CL12               'FILE TOTALS'                                
MCPY     DS    CL9                 'COMPANY'                                    
         DS    CL1                                                              
MCODE    DS    CL2                 COMPANY CODE                                 
         SPACE 1                                                                
         EJECT                                                                  
ACNVD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACNVWORK                                                       
         PRINT ON                                                               
* ACNVDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACNVDSECT                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACNV04    08/16/00'                                      
         END                                                                    
