*          DATA SET ACEXTFIX   AT LEVEL 058 AS OF 05/01/02                      
*PHASE ACEXTFIX,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'ACCOLADE EXTERNAL - GENERAL FIX ROUTINE'                        
ACEXTFIX CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*ACFIX*,RR=R7                                                  
         LR    R9,R1                                                            
         USING ACCWORKD,R9                                                      
         L     R2,AIOAREA                                                       
         LA    R3,4(R2)            SKIP 4 BYTE LENGTH                           
         USING ACKEYD,R3                                                        
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         CLI   OVSWITCH,0          TEST FIRST TIME HOOK                         
         BNE   ACC2                                                             
*                                                                               
         MVI   OVSWITCH,1                                                       
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R7                                                            
         ST    RE,PRNTBL                                                        
         L     RE,=V(DATCON)                                                    
         AR    RE,R7                                                            
         ST    RE,DATCON                                                        
         L     RE,=V(HEXOUT)                                                    
         AR    RE,R7                                                            
         ST    RE,HEXOUT                                                        
         B     XIT                                                              
         EJECT                                                                  
ACC2     CLI   OVSWITCH,1                                                       
         BNE   ACC100                                                           
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESS TRANSACTIONS                                                   
*-------------------------------------------------------------------*           
*                                                                               
         CLI   PROCREC,TRNSACTN                                                 
         BNE   XIT                                                              
*                                                                               
         CLC   ACKEYACC+1(2),=C'SB'                                             
         BNE   XIT                                                              
*                                                                               
         LA    R5,ACRECORD                                                      
         USING TRANSD,R5                                                        
         CLI   0(R5),X'44'                                                      
         BNE   XIT                                                              
         OC    ACDTPEEL,ACDTPEEL                                                
         BZ    XIT                                                              
         BAS   RE,DMPGET                                                        
         B     XIT                                                              
***                                                                             
         TM    TRNSSTAT,X'80'      DEBITS ONLY                                  
         BNO   XIT                                                              
*                                                                               
         LR    R4,R3               ADDRESS RECORD FOR GETEL                     
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
*                                                                               
         USING ACPERSD,R4                                                       
         CLI   ACPSLEN,X'0F'       STATUS IN ELEMENT                            
         BE    PT40                                                             
*                                                                               
         AP    SHORT,=P'1'                                                      
         B     XIT                                                              
*                                                                               
*        FIX BAD 40 ELEMENTS                                                    
*        IE. THOSE WITH MULTILPE TYPES OF TIME AND CRAP IN THE SPARE            
*                                                                               
PT40     TM    ACPSSTAT,ACPSBIL+ACPSNOT                                         
         BO    PT45                                                             
         TM    ACPSSTAT,ACPSRTE+ACPSNOT                                         
         BO    PT45                                                             
         TM    ACPSSTAT,ACPSBIL+ACPSRTE                                         
         BO    PT45                                                             
*                                                                               
         OC    ACPSSTAT+1(2),ACPSSTAT+1                                         
         BNZ   PT45                                                             
*                                                                               
         TM    ACPSSTAT,ACPSBIL    MAKE SURE ITS B TIME                         
         BO    XIT                 STATUS HAS BEEN SET                          
*                                                                               
PT45     MVC   PSAVE,SPACES                                                     
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R4),P40IN,15,0                                     
*                                                                               
         NI    ACPSSTAT,ACPSADJ         TURN OFF ALL BUT ADJ BIT                
         OI    ACPSSTAT,ACPSBIL                                                 
         XC    ACPSSTAT+1(2),ACPSSTAT+1 CLEAR OUT SPARE BYTES ALSO              
*                                                                               
         AP    CHARCD,=P'1'                                                     
*                                                                               
         CLI   TRNSTYPE,57                                                      
         BNE   *+14                                                             
         AP    CNT57,=P'1'                                                      
         B     PT50                                                             
*                                                                               
         CLI   TRNSTYPE,34                                                      
         BNE   *+14                                                             
         AP    CNT34,=P'1'                                                      
         B     PT50                                                             
*                                                                               
         AP    CNTOTH,=P'1'                                                     
*                                                                               
PT50     MVC   P,SPACES                                                         
         MVI   USESW,0                                                          
*                                                                               
*                                                                               
         GOTO1 HEXOUT,DMCB,ACKEYACC,PCOMP,1,0                                   
         GOTO1 HEXOUT,DMCB,TRNSTYPE,PTYPE,1,0                                   
         GOTO1 HEXOUT,DMCB,0(R4),P40OUT,15,0                                    
         MVC   PACC(14),ACKEYACC+1                                              
         MVC   PWRK,ACKEYWRK                                                    
         MVC   PCACC(14),ACKEYCON+1                                             
         MVC   PKREF(6),ACKEYREF                                                
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(5,PDATE)                               
         MVC   PBATCH,TRNSBTCH                                                  
*                                                                               
**       BAS   RE,DMPPUT                                                        
         MVC   P,PSAVE                                                          
         GOTO1 VPRINTER                                                         
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESSED LAST                                                         
*-------------------------------------------------------------------*           
ACC100   DS    0H                                                               
         B     XIT                                                              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         EDIT  (P6,CHARCD),(14,P+20),MINUS=YES                                  
         MVC   P+1(16),=C'BAD 40 ELS FIXED'                                     
         MVI   SPACING,1                                                        
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (P6,CNT34),(14,P+20),MINUS=YES                                   
         MVC   P+1(16),=C'TYPE 34''S       '                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (P6,CNT57),(14,P+20),MINUS=YES                                   
         MVC   P+1(16),=C'TYPE 57''S       '                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (P6,CNTOTH),(14,P+20),MINUS=YES                                  
         MVC   P+1(16),=C'OTHER           '                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (P6,SHORT),(14,P+20),MINUS=YES                                   
         MVC   P+1(16),=C'SHORT 40 ELEMENTS'                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        DUMP GETS AND PUTS                                                     
*-------------------------------------------------------------------*           
         USING ACKEYD,R4                                                        
DMPGET   NTR1                                                                   
         LA    R4,4(R2)                                                         
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1                                                                   
         LA    R4,4(R2)                                                         
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
         SPACE 1                                                                
DUMP     SR    R0,R0                                                            
         ICM   R0,3,ACLENGTH                                                    
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R4),C'DUMP',(R0),=C'2D',0                  
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              ROUTINE TO GET AN ELEMENT                                        
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*-------------------------------------------------------------------*           
FNDL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
*-------------------------------------------------------------------*           
*              ROUTINE TO DELETE AN ELEMENT                                     
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*-------------------------------------------------------------------*           
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 VHELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
*-------------------------------------------------------------------*           
*              ROUTINE TO ADD AN ELEMENT                                        
*              P1   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 1-3  A(ELEMENT)                                        
*-------------------------------------------------------------------*           
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),(R3),0                      
         B     XIT                                                              
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
PRNTBL   DS    A                                                                
DATCON   DS    A                                                                
HEXOUT   DS    A                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'1000'                                                        
CHA1A    DC    PL4'0'                                                           
CHA50    DC    PL4'0'                                                           
TOTDR    DC    PL4'0'                                                           
TOTCR    DC    PL4'0'                                                           
TOTREC   DC    PL4'0'                                                           
*                                                                               
CHARCD   DC    PL6'0'                                                           
CNT34    DC    PL6'0'                                                           
CNT57    DC    PL6'0'                                                           
CNTOTH   DC    PL6'0'                                                           
SHORT    DC    PL6'0'                                                           
*                                                                               
USESW    DC    CL1'0'                                                           
*                                                                               
ELCODE   DC    X'00'                                                            
DATADISP DC    H'49'                                                            
**ELEM     DS    CL255                                                          
*                                                                               
PSAVE    DS    0CL132                                                           
PCOMP    DS    CL2                                                              
         DS    CL1                                                              
PACC     DS    CL14                                                             
         DS    CL1                                                              
PWRK     DS    CL2                                                              
         DS    CL1                                                              
PCACC    DS    CL14                                                             
         DS    CL1                                                              
PKREF    DS    CL6                                                              
         DS    CL1                                                              
PDATE    DS    CL8                                                              
         DS    CL1                                                              
PTYPE    DS    CL2                                                              
         DS    CL1                                                              
PBATCH   DS    CL6                                                              
         DS    CL1                                                              
P40IN    DS    CL30                                                             
         DS    CL1                                                              
P40OUT   DS    CL30                                                             
         DS    CL1                                                              
         DS    CL1                                                              
         ORG   PSAVE+L'PSAVE                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACACCWORKD                                                     
         EJECT                                                                  
*        ACGENBOTH                                                              
*        DDDPRINT                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ACEXTFIX  05/01/02'                                      
         END                                                                    
