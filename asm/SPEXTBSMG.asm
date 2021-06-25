*          DATA SET SPEXTBSMG  AT LEVEL 024 AS OF 12/23/96                      
*          DATA SET SPEXTDFMG  AT LEVEL 014 AS OF 10/14/93                      
*          DATA SET SPEXTBS    AT LEVEL 003 AS OF 12/21/91                      
*PHASE SPEXTBSM                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE CLPACK                                                                 
         TITLE 'DMLDEXTBS - COPY BSNY TV MH6 MKT GRP RECS TO MH7'               
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
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         LA    R4,CLTABCT                                                       
         LA    R5,CLTABLE                                                       
DMXINIT1 GOTO1 =V(CLPACK),DMCB,(R5),WORK                                        
         CLC   WORK(2),3(R5)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(CLPACK),DMCB,5(R5),WORK                                       
         CLC   WORK(2),8(R5)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,CLTABLN(,R5)                                                  
         BCT   R4,DMXINIT1                                                      
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                 POINT TO RECORD                          
         CLI   0(R3),X'0D'                                                      
         BNE   DMXPURGE                                                         
         CLI   1(R3),X'03'                                                      
         BE    DMXREC10                                                         
         CLI   1(R3),X'02'                                                      
         BNE   DMXPURGE                                                         
         SPACE                                                                  
         CLI   2(R3),X'91'         BSZNY T                                      
         BE    *+12                                                             
         CLI   2(R3),X'92'         BSZNY R                                      
         BNE   DMXPURGE                                                         
         SPACE                                                                  
*        CLI   8(R3),C'D'          ONLY MARKET GROUP D                          
*        BNE   DMXPURGE                                                         
*                                                                               
         LA    R4,CLTABCT                                                       
         LA    R5,CLTABLE                                                       
DMXREC04 CLC   3(2,R5),3(R3)                                                    
         BE    DMXREC06                                                         
         LA    R5,CLTABLN(,R5)                                                  
         BCT   R4,DMXREC04                                                      
         B     DMXPURGE                                                         
         SPACE                                                                  
DMXREC06 MVC   3(2,R3),8(R5)           SET NEW CLT CODE                         
*        MVI   2(R3),X'12'                                                      
*        MVC   20(2,R3),=C'TH'                                                  
         ICM   R1,15,10(R5)                                                     
         LA    R1,1(,R1)                                                        
         STCM  R1,15,10(R5)                                                     
         B     PRT                                                              
         SPACE                                                                  
DMXREC10 CLI   8(R3),X'91'         BSZNY T                                      
         BE    *+12                                                             
         CLI   8(R3),X'92'         BSZNY R                                      
         BNE   DMXPURGE                                                         
         SPACE                                                                  
*        CLI   29(R3),C'D'         ONLY SCHEME D                                
*        BNE   DMXPURGE                                                         
         LA    R4,CLTABCT                                                       
         LA    R5,CLTABLE                                                       
DMXREC14 CLC   3(2,R5),9(R3)                                                    
         BE    DMXREC16                                                         
         LA    R5,CLTABLN(,R5)                                                  
         BCT   R4,DMXREC14                                                      
         B     DMXPURGE                                                         
         SPACE                                                                  
DMXREC16 MVC   9(2,R3),8(R5)           SET NEW CLT CODE                         
*        MVI   8(R3),X'12'                                                      
*        MVC   20(2,R3),=C'TH'                                                  
         ICM   R1,15,14(R5)                                                     
         LA    R1,1(,R1)                                                        
         STCM  R1,15,14(R5)                                                     
         SPACE                                                                  
         CLC   13(2,R3),=H'32'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   24(R3),05           BETTER BE ELEM                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   25(R3),08           BETTER BE LEN                                
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
PRT      DS    0H                                                               
         LA    R4,=CL20'MKT GRP/MKT GRP ASGN'                                   
         SR    R5,R5                                                            
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         LA    R4,CLTABCT                                                       
         LA    R5,CLTABLE                                                       
DMXEOF10 MVC   P+5(6),=C'CLIENT'                                                
         MVC   P+12(3),5(R5)                                                    
         MVC   P+16(9),=C'0D02 RECS'                                            
         EDIT  (B4,10(R5)),(6,P+26),ZERO=NOBLANK,COMMAS=YES                     
         MVC   P+35(9),=C'0D03 RECS'                                            
         EDIT  (B4,14(R5)),(6,P+44),ZERO=NOBLANK,COMMAS=YES                     
         GOTO1 VPRINTER                                                         
         LA    R5,CLTABLN(,R5)                                                  
         BCT   R4,DMXEOF10                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         LTORG                                                                  
*LTABLE  DC    CL3'MB6',XL2'B025',CL3'MB7',X'B026',XL4'00',XL4'00'              
*        DC    CL3'ML6',XL2'B165',CL3'ML7',X'B166',XL4'00',XL4'00'              
*        DC    CL3'MR6',XL2'B225',CL3'MR7',X'B226',XL4'00',XL4'00'              
CLTABLE  DS   0H                                                                
         DC    CL3'MH6',XL2'B0E5',CL3'MH7',X'B0E6',XL4'00',XL4'00'              
CLTABLN  EQU   *-CLTABLE                                                        
CLTABCT  EQU   (*-CLTABLE)/CLTABLN                                              
WORK     DC    XL64'00'                                                         
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
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
SPGENBUYD DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPEXTBSMG 12/23/96'                                      
         END                                                                    
