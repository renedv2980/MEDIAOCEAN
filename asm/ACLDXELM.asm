*          DATA SET ACLDXELM   AT LEVEL 016 AS OF 09/13/95                      
*PHASE ACLDXELM,*                                                               
*INCLUDE ACRECTYP                                                               
*INCLUDE HEXOUT                                                                 
         TITLE 'GENERAL ACLD EXTERNAL'                                          
*        COUNT ELEMENT ROUTINE                                                  
*--------------------------------------------------------------------*          
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*--------------------------------------------------------------------*          
         PRINT NOGEN                                                            
DMLDELS  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDELS                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
*--------------------------------------------------------------------*          
* CONTROL FLOW LOGIC                                                            
*--------------------------------------------------------------------*          
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
                                                                                
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
                                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
                                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
                                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
                                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
                                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
                                                                                
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*----------------------------------------------------------------*              
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*----------------------------------------------------------------*              
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*----------------------------------------------------------------*              
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*----------------------------------------------------------------*              
         USING RECTYPD,R3                                                       
         USING ACTRECD,R4                                                       
DMXREC   L     R4,AREC                                                          
         GOTO1 =V(ACRECTYP),DMCB,(C'D',ACTRECD)                                 
         CLI   0(R1),ACRTOFF       IS IT SPECIAL RECORD?                        
         BL    DMXKEEP                                                          
         LA    R2,CPYTAB                                                        
DMXREC05 CLI   0(R2),EOF           END OF TABLE?                                
         BE    DMXKEEP                                                          
         CLC   0(1,R2),1(R1)       COMPARE COMPANY CODE WITH TABLE              
         BE    *+12                                                             
         LA    R2,L'CPYTAB(R2)                                                  
         B     DMXREC05                                                         
*                                                                               
DMXREC10 SR    R1,R1                                                            
         IC    R1,0(R4)            PUT RECORD TYPE INTO R1 FOR COUNT            
         MH    R1,=Y(RECTYLNQ)     MULTIPLY BY 12 FOR PLACMENT IN TABLE         
         LA    R3,RECTAB(R1)       R3 = A(TABLE ENTRY)                          
         SR    R1,R1                                                            
         ICM   R1,1,1(R2)          WHICH COLUMN? R1 = LOOP COUNTER              
         BZ    *+12                IF 1ST COLUMN LEAVE OFFSET ALONE             
         LA    R3,4(R3)            BUMP TO NEXT COLUMN                          
         BCT   R1,*-4                                                           
*                                                                               
         L     RE,0(R3)            RE = COUNTER                                 
         LA    RE,1(RE)            ACCUMULATE COUNTER                           
         ST    RE,0(R3)                                                         
         B     DMXKEEP                                                          
         DROP  R3                                                               
         EJECT                                                                  
*----------------------------------------------------------------*              
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*----------------------------------------------------------------*              
DMXRET   DS    0H                                                               
                                                                                
*----------------------------------------------------------------*              
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*----------------------------------------------------------------*              
         USING RECTYPD,R2                                                       
         USING PLINED,R4                                                        
DMXEOF   DS    0H                                                               
         LA    R2,RECTAB           R2 = (RECORD COUNT TABLE)                    
         LA    R3,0                R3 = (RECORD TYPE)                           
         LA    R4,P                                                             
         XC    RECTYP,RECTYP                                                    
         AP    LINE,MAXLINE        FORCE NEW PAGE                               
*                                                                               
DMXEOF10 ZAP   PKLNCNT,=P'0'                                                    
         MVC   PHRECTP,=C'RECORD TYPE'                                          
         MVC   PHBBCNT,=C'BB'                                                   
         MVC   PHBDCNT,=C'BD'                                                   
         MVC   PHBPCNT,=C'BP'                                                   
         GOTO1 VPRINTER                                                         
         MVC   PHRECTP,=C'-----------'                                          
         MVC   PHBBCNT,=C'--'                                                   
         MVC   PHBDCNT,=C'--'                                                   
         MVC   PHBPCNT,=C'--'                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
DMXEOF20 CLI   RECTYP,X'40'                                                     
         BH    DMXEOFX                                                          
         GOTO1 HEXOUT,DMCB,RECTYP,PRECTYP,L'RECTYP,=C'TOG'                      
         EDIT  (4,RECBBCNT),(10,PBBCNT),ZERO=NOBLANK                            
         EDIT  (4,RECBDCNT),(10,PBDCNT),ZERO=NOBLANK                            
         EDIT  (4,RECBPCNT),(10,PBPCNT),ZERO=NOBLANK                            
         GOTO1 VPRINTER                                                         
         LA    R3,1(R3)            INCREMENT ELEMENT CODE                       
         STC   R3,RECTYP                                                        
         LA    R2,RECTYLNQ(R2)     BUMP TABLE                                   
         AP    PKLNCNT,=P'1'                                                    
         CP    PKLNCNT,MAXLN                                                    
         BNH   DMXEOF20            ONLY PRINT 51 LINES A PAGE                   
         AP    LINE,MAXLINE        FORCE NEW PAGE                               
         B     DMXEOF10                                                         
*                                                                               
DMXEOFX  AP    LINE,MAXLINE        FORCE NEW PAGE                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
CPYTAB   DS    0CL2                                                             
         DC    X'82',X'0'          BBDO - BB                                    
         DC    X'F5',X'1'          BBDO - BD                                    
         DC    X'56',X'2'          BBDO - BP                                    
         DC    AL1(EOF)                                                         
*                                                                               
RECTAB   DS    210F                RECORD TABLE ('00' - '40')                   
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE AND COSTANTS                                       *          
**********************************************************************          
         SPACE 1                                                                
HEXOUT   DC    V(HEXOUT)                                                        
UNDERLIN DC    V(UNDERLIN)                                                      
*                                                                               
MAXLN    DC    PL4'25'             MAXIMUM LINES PER PAGE                       
PKLNCNT  DS    PL4                                                              
EOF      EQU   X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* RECORT TYPE DSECT                                                  *          
**********************************************************************          
         SPACE 1                                                                
RECTYPD  DSECT                                                                  
RECBBCNT DS    F                   BBDO - BB COUNT                              
RECBDCNT DS    F                   BBDO - BD COUNT                              
RECBPCNT DS    F                   BBDO - BP COUNT                              
RECTYLNQ EQU   *-RECTYPD                                                        
         EJECT                                                                  
**********************************************************************          
* PRINT LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL10                                                             
PHRECTP  DS    CL11                RECORD TYPE HEADING                          
         DS    CL16                                                             
PHBBCNT  DS    CL2                 BBDO - BB COLUMN HEADING                     
         DS    CL10                                                             
PHBDCNT  DS    CL2                 BBDO - BD COLUMN HEADING                     
         DS    CL10                                                             
PHBPCNT  DS    CL2                 BBDO - BP COLUMN HEADING                     
         ORG   PLINED                                                           
         DS    CL16                                                             
PRECTYP  DS    CL2                 RECORD TYPE                                  
         DS    CL11                                                             
PBBCNT   DS    CL10                BBDO - BB COUNT                              
         DS    CL2                                                              
PBDCNT   DS    CL10                BBDO - BD COUNT                              
         DS    CL2                                                              
PBPCNT   DS    CL10                BBDO - BP COUNT                              
PTLNQ    EQU   *-PLINED                                                         
         EJECT                                                                  
**********************************************************************          
* OTHER DSECTS                                                       *          
**********************************************************************          
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL64                                                             
                                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELEMENT  DS    XL255                                                            
RECTYP   DS    CL1                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACLDXELM  09/13/95'                                      
         END                                                                    
