*          DATA SET CTPROFREP  AT LEVEL 002 AS OF 03/15/93                      
*PHASE PROFREP,*                                                                
***********************************************************************         
*                                                                     *         
* MODULE CALLED AS AN EXTERN TO CONCRETE TO REPORT ON OFFICE LEVEL    *         
* PROFILES FOR SPOT TN, TA, T0 WITH MEDIA N (NET PROFILES)            *         
*                                                                     *         
***********************************************************************         
         TITLE 'CTPROFREP - PROFILE REPORT'                                     
PROFCH   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REPT**                                                       
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
*                                                                               
         MVI   DATADISP+1,28                                                    
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
         USING CTUREC,R2                                                        
*                                                                               
         CLI   CTUKTYP,CTUKTYPQ    IS THIS A PROFILE?                           
         BNE   EXIT                                                             
*                                                                               
         TM    27(R2),X'80'        IS THE FUCKING THING DELETED?                
         BNZ   EXIT                WHY THE FUCK AM I HERE ON SUNDAY???          
*                                                                               
         CLI   CTUKSYS,C'S'        SYSTEM SPOT?                                 
         BNE   EXIT                                                             
         CLC   CTUKPROG+1(2),=C'TN'                                             
         BE    M10                                                              
         CLC   CTUKPROG+1(2),=C'TA'                                             
         BE    M10                                                              
         CLC   CTUKPROG+1(2),=C'T0'                                             
         BE    M10                                                              
         B     EXIT                                                             
*                                                                               
M10      DS    0H                                                               
*         CLI   CTUKMED,C'N'        IS THIS A NET PROFILE?                      
*         BNE   EXIT                 NO                                         
         CLI   CTUKCLT,C'*'        IS THIS AN OFFICE PROFILE?                   
         BNE   EXIT                 NO                                          
*                                                                               
* PRINT OUT KEYS OF RECS AFFECTED                                               
*         GOTO1 VHEXOUT,DMCB,0(R2),P,25,=C'TOG'                                 
*         GOTO1 VPRINTER                                                        
         MVC   P+00(02),CTUKPROG+1                                              
         MVC   P+03(02),CTUKAGY                                                 
         MVC   P+06(01),CTUKMED                                                 
         MVC   P+08(03),CTUKCLT                                                 
         GOTO1 VPRINTER                                                         
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
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTPROFREP 03/15/93'                                      
         END                                                                    
