*          DATA SET CTCONSPLIT AT LEVEL 021 AS OF 05/06/87                      
*PHASE CONSPLIT,*                                                               
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO DETERMINE FOR EACH        *         
* RECORD WHETHER IT BELONGS ON THE FILE CURRENTLY BEING LOADED.       *         
* FILE DTF NAME MUST BE 'CTFILE' OR 'CTUSER.'                         *         
* RECORDS NOT MATCHING LOAD FILE NAME ARE DELETED.                    *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONSPLIT - SPLIT CONTROL FILE FOR USER RECS'                  
CONSPLIT CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CONSPLIT                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
         CLI   OVSWITCH,X'FF'      TEST LAST                                    
         BE    EXITX                                                            
*                                                                               
PROC     MVI   FILE,C'U'           PRESET CTUSER                                
*                                                                               
         L     R6,AIOAREA          POINT TO RECORD                              
         LA    R6,4(R6)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R6),X'FF'         TEST FILE TRAILER                            
         BE    EXIT                YES - NEVER PURGE                            
*                                                                               
PROC2    LA    R1,CTUSRTAB                                                      
         LA    R0,(CTUSRTBX-CTUSRTAB)/L'CTUSRTAB                                
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
PROC20   LA    R1,L'CTUSRTAB(R1)                                                
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
EXIT     DS    0H                                                               
         CLC   LASTTYPE,0(R6)      TEST SAME AS LAST ONE PRINTED                
         BE    EXITX               YES - IGNORE                                 
         MVC   LASTTYPE,0(R6)                                                   
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   P(1),FILE                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R6),P+2,27,=C'TOG'                                 
         GOTO1 VPRINTER                                                         
EXITX    XIT1                                                                   
*                                                                               
LASTTYPE DC    X'00'                                                            
         EJECT                                                                  
******************************************************************              
*                                                                *              
* TABLE ENTRIES USED TO DETERMINE IF CTFILE RECORD RESIDES ON    *              
* CTFILE OR CTUSER.                                              *              
*                                                                *              
* BYTES 0   RECTYPE                                              *              
*                                                                *              
* BYTE  1   U=CTUSER,F=CTFILE IF REC MATCHES 4-7                 *              
*       2-7 DSPL/LEN/VALUE                                       *              
*                                                                *              
******************************************************************              
         SPACE 1                                                                
         DS    0D                                                               
CTUSRTAB DS    0XL8                                                             
         SPACE 1                                                                
         DC    X'01'               SAVED SCREENS                                
         DC    7X'00'                                                           
*                                                                               
         DC    C'0'                PERSONAL AUTH                                
         DC    7X'00'                                                           
*                                                                               
         DC    C'U'                USER PROFILE (AND FIELD RECORDS)             
         DC    C'F',AL1(17,2,00,00)  TO CTFILE IF +16(2)=X'0000'                
         DC    2X'00'                                                           
*                                                                               
         DC    C'X'                CPP EXTRACT                                  
         DC    7X'00'                                                           
*                                                                               
         DC    C'Y'                CPP PROJECTION                               
         DC    7X'00'                                                           
*                                                                               
CTUSRTBX EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTCONWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021CTCONSPLIT05/06/87'                                      
         END                                                                    
