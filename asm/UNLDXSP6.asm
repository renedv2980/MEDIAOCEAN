*          DATA SET UNLDXSP6   AT LEVEL 013 AS OF 04/17/01                      
*PHASE UNXSPT6A                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'UNLDXSP6 - SCAN NON-ALPHA PROGRAMS'                             
*                                                                               
**********************************************************************          
*        THIS EXTERN PRINTS ALL PROGRAM NAMES THAT ARE NOT VALID                
*        ALPHA NUMERIC CHARACTERS (X'0D20' RECORDS)                             
**********************************************************************          
*                                                                               
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
         SPACE 2                                                                
         PRINT NOGEN                                                            
UNLDXSP  CSECT                                                                  
         NMOD1 WORKX-WORKD,UNLDXSP,RR=R2                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
* SCAN ALL '0D20' RECS FOR NON-ALPHANUMERIC PROGRAM NAMES                       
***********************************************************************         
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
         USING NPGRECD,R3                                                       
         CLC   0(2,R3),=X'0D20'    PROGRAM RECORD                               
         BNE   DMXKEEP                                                          
         GOTO1 =V(DATCON),DMCB,(2,NPGKEND),(3,DTFILT)                           
         CLC   DTFILT,=XL3'630C1A' IGNORE BEFORE 12/26/99                       
         BNH   DMXKEEP                                                          
*                                                                               
         LA    R4,NPGKPROG                                                      
         LA    R5,6                                                             
*                                                                               
DMX10    DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BE    DMX20                                                            
         CLI   0(R4),C'A'          NOT ALPHA NUMERIC?                           
         BL    DMX100                                                           
         CLI   0(R4),C'9'          NOT ALPHA NUMERIC?                           
         BH    DMX100                                                           
*                                                                               
DMX20    LA    R4,1(R4)                                                         
         BCT   R5,DMX10                                                         
*                                                                               
         LA    R4,NPGKPROG+5       CHECK FOR SPACES IN THE MIDDLE               
         LA    R5,6                                                             
         MVI   MYFLAG,0                                                         
*                                                                               
DMX30    DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BE    *+12                                                             
         OI    MYFLAG,NOSPACE                                                   
         B     DMX40                                                            
*                                                                               
         TM    MYFLAG,NOSPACE                                                   
         BO    DMX100                                                           
*                                                                               
DMX40    SHI   R4,1                                                             
         BCT   R5,DMX30                                                         
         B     DMXKEEP                                                          
*                                                                               
DMX100   DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,NPGKEY,P,13,=C'TOG'                              
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+5(6),NPGKPROG                                                  
         GOTO1 =V(DATCON),DMCB,(2,NPGKEND),(11,P+13)                            
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
         MVC   P(30),=C'# OF NON-ALPHANUMERIC PROGRAMS'                         
         L     R4,COUNT                                                         
         EDIT  (R4),(10,P+32)                                                   
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
                                                                                
         EJECT                                                                  
* *******************************************************************           
* VARIABLE LIST                                                                 
* *******************************************************************           
DATADISP DC    H'0024'                                                          
COUNT    DC    F'0'                                                             
COUNTER  DC    F'30'                                                            
DTFILT   DS    XL3                                                              
MYFLAG   DS    XL1                                                              
NOSPACE  EQU   X'01'               NO SPACE HERE                                
*                                                                               
         SPACE                                                                  
*                                                                               
         SPACE                                                                  
         LTORG                                                                  
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
BYTE     DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENPROG                                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENSLST                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013UNLDXSP6  04/17/01'                                      
         END                                                                    
