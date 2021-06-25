*          DATA SET CTCONSIZE  AT LEVEL 035 AS OF 05/01/02                      
*PHASE CONSIZEA                                                                 
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO FIND ID RECS OVER 1000    *         
* BYTES                                                               *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTCONAUTH - CHANGE SYS AUTH (X''21'') ELEMS'                    
CONSIZE  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*CONSIZE                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BNE   M10                 YES - PRINT TOTALS                           
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   P(10),=C'CHANGED:  '                                             
         GOTO1 VHEXOUT,DMCB,COUNT1,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'ALL=800F: '                                             
         GOTO1 VHEXOUT,DMCB,COUNT2,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'RRGO=:    '                                             
         GOTO1 VHEXOUT,DMCB,COUNT3,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=C'DELETED:  '                                             
         GOTO1 VHEXOUT,DMCB,COUNT4,P+10,4,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         DROP  R8                                                               
         B     EXIT                                                             
*                                                                               
M10      DS    0H                                                               
**************** DELETE THE NEXT LINE - TESTING ONLY ****************           
*         MVI   WRITE,X'FF'         ***** HEY, LOOK AT ME *****                 
**************** DELETE THE PREV LINE - TESTING ONLY ****************           
*                                                                               
M15      CLI   0(R2),C'I'          TEST ID RECORD                               
         BNE   EXIT                NO - LEAVE ALONE                             
         OC    1(24,R2),1(R2)      NULL KEY? (HI TERM REC?)                     
         BZ    EXIT                                                             
*                                                                               
         USING CTIREC,R2                                                        
         TM    CTISTAT,X'80'       IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
         EJECT                                                                  
*                                                                               
         LR    R6,R2                                                            
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                TERM HAS NO VALID ID                         
         CLC   =C'YN',2(R6)                                                     
         BNE   EXIT                                                             
*                                                                               
         CLC   CTILEN,=H'1000'     REC >= 1000 BYTES?                           
         BNL   PRNERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
PRNERR   DS    0H                                                               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,23(R2),P+14,2,=C'TOG'                               
         MVC   P+20(10),14(R2)                                                  
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         DROP  R8                                                               
                                                                                
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
COUNT1   DC    F'0'                CHANGED RECS                                 
COUNT2   DC    F'0'                ALL=800F                                     
COUNT3   DC    F'0'                RRGO=                                        
COUNT4   DC    F'0'                DELETED C'5'S                                
*                                                                               
ELEM     DS    CL255                                                            
*                                                                               
SYSPRG   DS    0H                                                               
         DC    X'06'               ACC SYSTEM                                   
         DC    X'030264'            FILE=0264                                   
         DC    X'0B0464'            PRD=0464                                    
         DC    X'190000'            INT=N                                       
         DC    X'00'               END SYSTEM ACC                               
*                                                                               
         DC    X'02'               SPOT SYSTEM                                  
         DC    X'130000'            PAY                                         
         DC    X'00'               END SYSTEM SPOT                              
*                                                                               
         DC    X'04'               PRINT SYSTEM                                 
         DC    X'030000'            PAY                                         
         DC    X'00'               END SYSTEM PRINT                             
*                                                                               
         DC    X'03'               NET SYSTEM                                   
         DC    X'130000'            PAY                                         
         DC    X'00'               END SYSTEM NET                               
*                                                                               
         DC    X'FF'               EOFT                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035CTCONSIZE 05/01/02'                                      
         END                                                                    
