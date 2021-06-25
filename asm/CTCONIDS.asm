*          DATA SET CTCONIDS   AT LEVEL 002 AS OF 11/24/87                      
*PHASE CONIDS,*                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO ELIMINATE 'SJR' AND       *         
* 'TCH1' FROM USERID COMPATIBLE LISTS.                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONIDS - PURGE DDS IDS FROM ID AND TERMINAL RECS'             
CONIDS   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CONIDS                                                         
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
         CLI   OVSWITCH,X'FF'      TEST LAST                                    
         BE    EXITX                                                            
*                                                                               
         L     R6,AIOAREA          POINT TO RECORD                              
         LA    R6,4(R6)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R6),X'FF'         TEST FILE TRAILER                            
         BE    EXIT                YES - NEVER PURGE                            
*                                                                               
         LA    R1,CTIDTAB                                                       
         LA    R0,(CTIDTBX-CTIDTAB)/L'CTIDTAB                                   
*                                                                               
PROC10   CLC   0(1,R1),0(R6)       MATCH RECTYPE                                
         BNE   PROC20                                                           
         CLI   1(R1),0             TEST SECONDARY CONDITION                     
         BE    PROCX               NO                                           
*                                                                               
         MVC   FILE(1),1(R1)       SET FILE IF MATCH CONDITION                  
         ZIC   RE,2(R1)            GET DSPL                                     
         AR    RE,R6               POINT TO DATA                                
         ZIC   RF,3(R1)            GET LENGTH                                   
         BCTR  RF,0                SET FOR EX                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   4(0,R1),0(RE) *EXECUTED*                                         
         BE    PROCX                                                            
* NO MATCH - SWITCH FILE                                                        
         CLI   FILE,C'U'                                                        
         BE    PROC25                                                           
         MVI   FILE,C'U'                                                        
         B     PROCX                                                            
*                                                                               
PROC20   LA    R1,L'CTIDTAB(R1)                                                 
         BCT   R0,PROC10                                                        
*                                                                               
PROC25   MVI   FILE,C'F'           NO MATCH - DEFAULT TO CTFILE                 
*                                                                               
PROCX    L     RE,ACTFILE          GET DTF ADDRESS                              
         CLC   FILE,22+2(RE)       TEST LOAD FILE = MY FILE                     
         BE    EXIT                YES - KEEP                                   
         MVI   WRITE,X'FF'         ELSE SET TO DELETE RECORD                    
         B     EXIT                                                             
FILE     DC    X'00'                                                            
         EJECT                                                                  
EXIT     CLC   LASTTYPE,0(R6)      TEST SAME AS LAST ONE PRINTED                
         BE    EXITX               YES - IGNORE                                 
         MVC   LASTTYPE,0(R6)                                                   
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   P(1),FILE                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R6),P+2,27,=C'TOG'                                 
         GOTO1 VPRINTER                                                         
EXITX    XIT1                                                                   
         EJECT                                                                  
LASTTYPE DC    X'00'                                                            
*                                                                               
         DS    0D                                                               
CTIDTAB                                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTCONWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTCONIDS  11/24/87'                                      
         END                                                                    
