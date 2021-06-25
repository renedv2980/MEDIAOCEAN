*          DATA SET SPLDPROG   AT LEVEL 050 AS OF 08/16/99                      
*          DATA SET SPLDBILL   AT LEVEL 010 AS OF 06/04/99                      
*PHASE SPLDPR                                                                   
*INCLUDE RECUP                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'DMLDEXTZEN - ADD 61 ELEMENT FOR ZENITH'                         
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         USING NPGRECD,R6                                                       
         CLC   NPGKTYP,=XL2'0D20'                                               
         BNE   DMXKEEP                                                          
*        CLI   NPGKAM,X'63'                                                     
*        BNE   DMXKEEP                                                          
*                                                                               
         CLI   NPGKPROG+3,C'0'      CHECK NAME+3 FOR CHARACTER                  
         BL    DMXKEEP                                                          
         CLI   NPGKPROG+3,C'9'                                                  
         BH    DMXKEEP                                                          
*                                                                               
         CLI   NPGKPROG+4,C'0'      CHECK NAME+4 FOR CHARACTER                  
         BL    DMXKEEP                                                          
         CLI   NPGKPROG+4,C'9'                                                  
         BH    DMXKEEP                                                          
*                                                                               
         CLI   NPGKPROG+5,C'0'      CHECK NAME+5 FOR CHARACTER                  
         BL    DMXKEEP                                                          
         CLI   NPGKPROG+5,C'9'                                                  
         BH    DMXKEEP                                                          
*                                                                               
         CLC   NPGKEND,=XL2'C79F'                                               
         BNE   DMXKEEP                                                          
*        GOTO1 =V(PRNTBL),DMCB,=C'BEFOR',0(R6),C'DUMP',20,=C'1D'                
         MVC   NPGKEND,=XL2'F19E'   CHANGE DATE DEC30/20                        
*        GOTO1 =V(PRNTBL),DMCB,=C'AFTER',0(R6),C'DUMP',20,=C'1D'                
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(5),=C'COUNT'                                                   
         EDIT  (4,COUNT),(8,P+10),ZERO=NOBLANK                                  
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R5,24,ELCODE                                                     
*                 BBDO/CIMNY                                                    
SPTFILE  DC    CL8'SPTFILE'                                                     
*                                                                               
MKTTABLE DC    XL4'00010001'          ABC                                       
         DC    XL4'00020002'          CBS                                       
         DC    XL4'00030004'          NBC                                       
         DC    XL4'00190003'          FOX                                       
         DC    XL4'00D60007'          WBN                                       
         DC    XL4'00D707ED'          UPN                                       
         DC    X'FF'                                                            
*                                                                               
ELEMENT  DC    XL6'6106B2650100'                                                
COUNT    DS    F                   NUMBER OF RECORDS CHANGED                    
ELCODE   DS    X                                                                
PRCOUNT  DS    F                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
VHEXOUT  DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
BILLHDR  DSECT                                                                  
       ++INCLUDE SPGENPROG                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050SPLDPROG  08/16/99'                                      
         END                                                                    
