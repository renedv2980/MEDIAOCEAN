*          DATA SET SXLDEXTYKM AT LEVEL 127 AS OF 10/27/00                      
*          DATA SET SPLDEXTAN2 AT LEVEL 075 AS OF 08/28/97                      
*PHASE SXEXTYKM,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE DATCON                                                                 
         TITLE 'FIX NEW INVOICE RECORDS FOR MATCHMAKER'                         
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
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
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
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
***********    PUT CODE HERE                                                    
*                                                                               
         L     R6,AREC             POINT TO RECORD                              
         LR    R3,R6               FOR RECUP                                    
         USING SNVKEYD,R6                                                       
         CLC   =X'0E03',0(R6)      INVOICE REC ?                                
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   SNVKMINK,EFFS       PROCESS RECS WITH FFS IN EL KEY              
         BNE   DMXKEEP             WILL PREVENT REPETITION                      
         AP    TOTRECS,=P'1'                                                    
         CLC   SNVKMOS,JANOOC      IS DATE AFTER JAN1/00 ?                      
         BH    DMXKEEP             IF NOT - IGNORE                              
         AP    TOTRECDT,=P'1'                                                   
         CLC   SNVRLEN,=H'3850'    MAKE SURE REC WOUN'T SPLIT                   
         BH    DMXKEEP             WHEN WE ADD 100 BYTES                        
         AP    TOTRECLN,=P'1'                                                   
*                                                                               
DMXREC10 DS    0H                                                               
         MVI   ELCODE,X'E8'          FIND E8 ELEM                               
         BAS   RE,GETEL                                                         
         BNE   DMXREC20              NO, LOOK FOR FE ELEM                       
         AP    E8FOUND,=P'1'                                                    
         B     DMXKEEP               YES, THEN DONE                             
*                                                                               
DMXREC20 DS    0H                                                               
         L     R6,AREC             POINT TO RECORD                              
         MVI   ELCODE,X'FE'          FIND FE ELEM                               
         BAS   RE,GETEL                                                         
         BNE   DMXREC30              NO, ADD ONE                                
         AP    FEFOUND,=P'1'                                                    
         B     DMXKEEP               YES, THEN DONE                             
*                                    ADD FE ELEM FOR LEN OF 100                 
DMXREC30 XC    WORK,WORK             CLEAR MINELEM AREA                         
         MVI   WORK,X'FE'            ELEMENT CODE                               
         MVI   WORK+1,100            ELEMENT LENGTH                             
         GOTO1 =V(RECUP),DMCB,(C'T',(R3)),WORK,(R6)                             
         AP    FEADDED,=P'1'                                                    
***      BAS   RE,RECPRNT                                                       
*                                                                               
         B     DMXKEEP                                                          
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
RECPRNT  NTR1                                                                   
         CP    FEADDED,=P'10'                                                   
         BH    RPX                                                              
         LA    R4,=CL20'CHANGED REC'                                            
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,32(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
RPX      XIT1                                                                   
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(21),=C'NUMBER OF RECS READ ='                                  
         EDIT  TOTRECS,(16,P+30),ZERO=NOBLANK,COMMAS=YES                        
         GOTO1 VPRINTER                                                         
         MVC   P(28),=C'NUMBER OF RECS WITHIN DATE ='                           
         EDIT  TOTRECDT,(16,P+30),ZERO=NOBLANK,COMMAS=YES                       
         GOTO1 VPRINTER                                                         
         MVC   P(26),=C'NUMBER OF RECS PROCESSED ='                             
         EDIT  TOTRECLN,(16,P+30),ZERO=NOBLANK,COMMAS=YES                       
         GOTO1 VPRINTER                                                         
         MVC   P(19),=C'E8 ELEMENTS FOUND :'                                    
         EDIT  E8FOUND,(16,P+30),COMMAS=YES,ZERO=NOBLANK                        
         GOTO1 VPRINTER                                                         
         MVC   P(19),=C'FE ELEMENTS FOUND :'                                    
         EDIT  FEFOUND,(16,P+30),COMMAS=YES,ZERO=NOBLANK                        
         GOTO1 VPRINTER                                                         
         MVC   P(19),=C'FE ELEMENTS ADDED :'                                    
         EDIT  FEADDED,(16,P+30),COMMAS=YES,ZERO=NOBLANK                        
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DATADISP DC    H'0042'                                                          
*                                                                               
EFFS     DC    XL6'FFFFFFFFFFFF'                                                
JANOOC   DC    XL2'37DE'           COMPRESSED COMPLIMENT JAN1/00                
E8FOUND  DC    PL8'0'              COUNTER FOR FOUND E8 ELEMS                   
FEFOUND  DC    PL8'0'              COUNTER FOR FOUND FE ELEMS                   
FEADDED  DC    PL8'0'              COUNTER FOR ADDED FE ELEMS                   
TOTRECS  DC    PL8'0'              COUNTER FOR TOTAL RECS READ                  
TOTRECDT DC    PL8'0'              COUNTER FOR RECS WITHIN DATE                 
TOTRECLN DC    PL8'0'              COUNTER FOR RECS W/ LEN < 3850               
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
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
WORK     DS    CL100                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'127SXLDEXTYKM10/27/00'                                      
         END                                                                    
