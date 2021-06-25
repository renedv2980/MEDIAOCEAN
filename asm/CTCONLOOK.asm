*          DATA SET CTCONLOOK  AT LEVEL 004 AS OF 10/13/89                      
*PHASE CONLOOK,*                                                                
CONLOOK  TITLE '- LOOK AT ID RECORDS FOR ACCOUNTING'                            
CONLOOK  CSECT                                                                  
         NMOD1 0,**LOOK**                                                       
         USING CONWORKD,R6                                                      
         L     R2,AIOAREA                                                       
         LA    R2,4(R2)                                                         
         USING CTIREC,R2                                                        
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
*                                                                               
         CLI   OVSWITCH,X'FF'                                                   
         BE    CONEXT                                                           
         CLI   OVSWITCH,0                                                       
         BNE   CON2                                                             
         MVI   OVSWITCH,1                                                       
         B     CONEXT                                                           
*                                                                               
CON2     CLI   0(R5),C'E'                                                       
         BNE   CONEXT                                                           
         TM    27(R5),X'80'                                                     
         BO    CONEXT                                                           
         CLC   23(1,R5),LASYS                                                   
         BE    CON4                                                             
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'99'                                                      
         MVC   LASYS,23(R5)                                                     
*                                                                               
CON4     SR    R3,R3                                                            
         IC    R3,23(R5)                                                        
         EDIT  (R3),(2,P+22)                                                    
         OI    P+23,X'F0'                                                       
         IC    R3,24(R5)                                                        
         EDIT  (R3),(3,P+30)                                                    
         LA    R5,28(R5)                                                        
         IC    R3,1(R5)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+37(0),2(R5)                                                    
         GOTO1 PRINTER                                                          
*                                                                               
CONEXT   XMOD1 1                                                                
         EJECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL20                                                             
LASYS    DS    CL1                                                              
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTCONEXDS                                                      
