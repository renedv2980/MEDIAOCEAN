*          DATA SET ACREPXX02  AT LEVEL 007 AS OF 08/16/00                      
*PHASE ACXX02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'READ AND UPDATE BATCH HEADER RECORDS'                           
*                                                                               
*                                                                               
ACXX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXX**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXYD,RC                                                         
         EJECT                                                                  
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         ZAP   CASHA,=P'0'                                                      
         ZAP   CASHC,=P'0'                                                      
         ZAP   CASHD,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY)                                      
*                                                                               
         XC    DKEY,DKEY                                                        
         LA    R5,DKEY                                                          
         USING TBARECD,R5                                                       
         MVI   TBAKTYP,TBAKTYPQ    SET KEY FOR TRANS BATCH RECORDS              
*                                                                               
         L     R4,ADCOMP                                                        
         MVC   TBAKCPY,0(R4)       GET COMPANY CODE                             
*                                                                               
         BAS   RE,HIGH                                                          
         B     *+8                                                              
RS00     BAS   RE,SEQ                                                           
*                                                                               
         LA    R5,DIR                                                           
         CLI   TBARECD,TBAKTYPQ    HAS TO BE TRANS BATCH RECORD                 
         BNE   XIT                 I WON'T BE BACK                              
*                                                                               
         CLI   TBAKCPY,X'B3'        SAME COMPANY?                               
         BNE   XIT                                                              
         OC    TBAKTSEQ,TBAKTSEQ    IS IT A HEADER?                             
         BNZ   RS00                                                             
         OC    TBAHKUDT,TBAHKUDT    DOES IT HAVE UPDATE DATE?                   
         BZ    RS00                 NO,  SKIP IT                                
         TM    TBAKHSTA,TBAHSSAV    SAVED?                                      
         BO    RS00                 SKIP IT                                     
         TM    TBAKHSTA,TBAHSEND+TBAHSAPR    ENDED & APPROVED                   
         BNO   RS00                          NO, SKIP IT                        
         TM    TBAKHSTA,TBAHSUPD   IS UPDATE BIT ON                             
         BO    RS00                YES, IT'S OK                                 
         CLC   TBAHKEDT,TODAY      EFFECTIVE AFTER TODAY?                       
         BH    RS00                YES, SKIP IT                                 
         BAS   RE,DMPGDIR                                                       
         B     RS00                                                             
*                                                                               
         BNO   RS00                                                             
         CLC   TBAHKEDT,TODAY                                                   
         BH    RS00                                                             
         BAS   RE,DMPGDIR                                                       
         B     RS00                                                             
         MVC   DKEYSV,DIR                                                       
         L     R3,AIO2             GET THE RECORD AND DUMP IT                   
         BAS   RE,GET                                                           
*                                                                               
         MVI   REPORTSW,C'B'                                                    
         CLI   QOPT1,C'B'          PRINT A REPORT BEFORE?                       
         BNE   *+8                                                              
         BAS   RE,REPORTIT         YES                                          
*                                                                               
         L     R5,AIO2             GET THE ELEMENT                              
         LA    R2,TBARFST                                                       
*                                                                               
         OC    TBAKTSEQ,TBAKTSEQ   IS THIS A HEADER?                            
         BNZ   RS05                NO, LOOK FOR OTHER ELEMENT                   
*                                                                               
         USING BHDELD,R2                                                        
RS02     CLI   0(R2),0                                                          
         BE    RS06                                                             
         CLI   0(R2),BHDELQ                                                     
         BE    RS04                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     RS02                                                             
*                                                                               
RS04     ZAP   DUB,BHDCASHC        ADJUST THE AMOUNTS FOR 2 DECIMALS            
         MP    DUB,=P'100'                                                      
         ZAP   BHDCASHC,DUB                                                     
         AP    CASHC,BHDCASHC                                                   
*                                                                               
         ZAP   DUB,BHDCASHA                                                     
         MP    DUB,=P'100'                                                      
         ZAP   BHDCASHA,DUB                                                     
         AP    CASHA,BHDCASHA                                                   
*                                                                               
         ZAP   DUB,BHDTOTCR                                                     
         MP    DUB,=P'100'                                                      
         ZAP   BHDTOTCR,DUB                                                     
         AP    TOTCR,BHDTOTCR                                                   
         B     RS06                                                             
*                                                                               
         USING BIAELD,R2                                                        
RS05     CLI   0(R2),0                                                          
         BE    RS06                                                             
         CLI   0(R2),BIAELQ                                                     
         BE    RS05A                                                            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     RS05                                                             
*                                                                               
RS05A    ZAP   DUB,BIAAMT                                                       
         MP    DUB,=P'100'                                                      
         ZAP   BIAAMT,DUB                                                       
         AP    CASHD,BIAAMT                                                     
*                                                                               
RS06     DS    0H                                                               
         BAS   RE,PUT              WRITE RECORD BACK                            
         BAS   RE,DMPPUT           DUMP IT                                      
         MVI   REPORTSW,C'A'                                                    
         BAS   RE,REPORTIT                                                      
         MVC   DKEY,DKEYSV                                                      
         BAS   RE,HIGH                                                          
         B     RS00                                                             
*                                                                               
RECAP    DS    0H                                                               
         GOTO1 ACREPORT                                                         
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         MVC   P(11),=C'** TOTAL **'                                            
         EDIT  CASHA,(14,PCASHA),2,MINUS=YES                                    
         EDIT  CASHC,(14,PCASHC),2,MINUS=YES                                    
         EDIT  CASHD,(14,PCASHD),2,MINUS=YES                                    
         EDIT  TOTCR,(14,PTOTCR),2,MINUS=YES                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
REPORTIT NTR1                                                                   
         L     R5,AIO2                                                          
         USING TBARECD,R5                                                       
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         GOTO1 HEXOUT,DMCB,TBAKCPY,PCMPNY,1                                     
         MVC   PIND,REPORTSW                                                    
         MVC   WORK(2),TBAKADDT                                                 
         XC    WORK(2),=X'FFFF'                                                 
         GOTO1 DATCON,DMCB,(2,WORK),(8,PDATE)                                   
         MVC   PREF,TBAKBREF                                                    
*                                                                               
         LA    R2,TBARFST                                                       
         CLI   0(R2),BHDELQ                                                     
         BNE   REPT02                                                           
*                                                                               
         USING BHDELD,R2                                                        
         EDIT  (P8,BHDCASHA),PCASHA,2,MINUS=YES                                 
         EDIT  (P8,BHDCASHC),PCASHC,2,MINUS=YES                                 
         EDIT  (P8,BHDTOTCR),PTOTCR,2,MINUS=YES                                 
         B     REPT04                                                           
*                                                                               
         USING BIAELD,R2                                                        
REPT02   CLI   0(R2),BIAELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P6,BIAAMT),PCASHD,2,MINUS=YES                                   
*                                                                               
REPT04   GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R3),DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP SOME RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
DMPGDIR  NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GETD'                                                      
         LA    RF,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DMPX                                                             
*                                                                               
DMPPDIR  NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'PUTD'                                                      
         LA    RF,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DMPX                                                             
*                                                                               
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET '                                                      
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT '                                                      
         B     DUMP                                                             
         SPACE 1                                                                
DUMP     L     R3,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,TIMRLEN-TIMRECD(R3)                                         
DMPX     CLI   QOPT2,C'D'                                                       
         BNE   XIT                                                              
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R3),C'DUMP',(RF),=C'2D'                    
XIT      XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
*        LITERALS                                                               
*****************************************                                       
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
         EJECT                                                                  
*****************************************                                       
*        EQUATES                                                                
*****************************************                                       
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   0                   END OF TABLE                                 
*                                                                               
CASHA    DC    PL8'0'                                                           
CASHC    DC    PL8'0'                                                           
CASHD    DC    PL8'0'                                                           
TOTCR    DC    PL8'0'                                                           
ITEMAMNT DC    PL6'0'                                                           
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'2000'                                                        
*                                                                               
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
DATVAL   DC    V(DATVAL)                                                        
HEXIN    DC    V(HEXIN)                                                         
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIO3     DC    A(IO3)                                                           
*                                                                               
DKEY     DS    CL(L'ACCKEY)                                                     
DKEYSV   DS    CL(L'ACCKEY)                                                     
DIR      DS    CL64                                                             
DA       DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IO1      DS    CL2000                                                           
IO2      DS    CL2000                                                           
IO3      DS    CL2000                                                           
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PCMPNY   DS    CL2                 COMPANY CODE                                 
         DS    CL3                                                              
PIND     DS    CL1                 BEFORE AFTER INDICATOR                       
         DS    CL2                                                              
PDATE    DS    CL8                 DATE                                         
         DS    CL2                                                              
PREF     DS    CL4                 REFERENCE                                    
         DS    CL2                                                              
PCASHA   DS    CL14                DR                                           
         DS    CL2                                                              
PCASHC   DS    CL14                CR                                           
         DS    CL2                                                              
PCASHD   DS    CL14                CR                                           
         DS    CL2                                                              
PTOTCR   DS    CL14                CR                                           
PLNQ     EQU   *-PLINE                                                          
*                                                                               
*                                                                               
ACXYD    DSECT                                                                  
ELCODE   DS    XL1                                                              
ELM      DS    CL255                                                            
REPORTSW DS    CL1                                                              
TODAY    DS    XL2                                                              
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPXX02 08/16/00'                                      
         END                                                                    
