*          DATA SET CTCONCOST  AT LEVEL 002 AS OF 03/13/00                      
*PHASE CONCOST,*                                                                
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO CHANGE X'BA' ELEMS FOR    *         
* SYSTEM SECURITY RECORDS (TYPE 'F')                                  *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONCOST-CHANGE SYS ACTION RECORD (X''BA'' ELEMENT)'           
CONCOST  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONCOST                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
MAIN     DS    0H                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BNE   M10                 YES - PRINT TOTALS                           
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MVC   P+10(12),=C'CHANGED RECS'                                        
         EDIT  (P4,RECCHG),(8,P),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
M10      DS    0H                                                               
         USING SAASREC,R2                                                       
         TM    SAASSTAT,X'80'      ALREADY DELETED?                             
         BO    EXIT                YES                                          
*                                                                               
         CLI   SAASTYP,SAASTYPQ    TYPE = C'F'                                  
         BNE   EXIT                                                             
         CLI   SAASSUB,SAASSUBQ    SUB = X'15'                                  
         BNE   EXIT                                                             
         CLI   SAASOVS,X'06'       SYSTEM = ACCOUNTING                          
         BNE   EXIT                                                             
         CLI   SAASPGM,X'1D'       PROGRAM = COST                               
         BNE   EXIT                                                             
*                                                                               
         USING SAMIXD,R3                                                        
         LA    R3,SAASDATA         FIRST ELEM                                   
*                                                                               
DMX10    CLI   SAMIXEL,0                                                        
         BE    EXIT                                                             
         CLI   SAMIXEL,X'BA'       ACTION ELEMENT                               
         BNE   DMX10NXT                                                         
         CLI   SAMIXRCD,X'09'      RECORD = HISTORY                             
         BE    DMX20                                                            
*                                                                               
DMX10NXT ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DMX10                                                            
*                                                                               
DMX20    TM    SAMIXACT,X'08'      ACTION = DELETE                              
         BZ    DMX10NXT                                                         
*                                                                               
         NI    SAMIXACT,X'F7'      TURN OFF DELETE                              
*                                                                               
         BAS   RE,PRTKEY                                                        
         BAS   RE,PRTELEM                                                       
*                                                                               
         AP    RECCHG,=P'1'                                                     
         B     EXIT                                                             
         EJECT                                                                  
PRTELEM  NTR1                                                                   
         ZIC   R4,1(R3)            LEN                                          
         GOTO1 VHEXOUT,DMCB,0(R3),P,(R4),=C'TOG'                                
         GOTO1 VPRINTER                                                         
PRTELX   XIT1                                                                   
*                                                                               
PRTKEY   NTR1                                                                   
         GOTO1 VHEXOUT,DMCB,0(R2),P,30,=C'TOG'                                  
         GOTO1 VPRINTER                                                         
         MVC   P(30),0(R2)                                                      
         GOTO1 VPRINTER                                                         
PRTKEYX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RECCHG   DC    PL4'0'              CHANGED RECS                                 
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTCONCOST 03/13/00'                                      
         END                                                                    
