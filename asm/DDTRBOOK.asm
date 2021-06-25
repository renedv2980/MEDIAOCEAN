*          DATA SET DDTRBOOK   AT LEVEL 179 AS OF 09/08/98                      
*PHASE GCHEBOOK                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE XSORT                                                                  
***********************************************************************         
*                                                                               
*                                                                               
* 1.  BE SURE TO CHANGE THE *PHASE CARD ABOVE SO YOU DON'T WIPE                 
*     OUT EACH OTHER'S LOAD MODULES.  REPLACE THE 'XXXX' WITH YOUR              
*     USERID.                                                                   
*                                                                               
* 2.  USE THE CVD AND UNPK INSTRUCTIONS TO PUT EACH BINARY NUMBER               
*     INTO EACH PRINT FIELD.                                                    
*                                                                               
* 3.  THERE IS JCL TO RUN THE PROGRAM IN 'DEIS.DDS.JCL(XXXXFIB)' --             
*     COPY THIS TO YOUR OWN JCL LIBRARY.                                        
*                                                                               
***********************************************************************         
         EJECT                                                                  
* LEAVE THIS CODE ALONE                                                         
*                                                                               
TESTHEX  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*TESTHEX*,=V(REGSAVE)                                          
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN10                                                           
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TESTHEX),V(DUMMY)                                              
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
MAIN10   DS    0H                                                               
         EJECT                                                                  
**********************                                                          
* PUT YOUR CODE HERE *                                                          
**********************                                                          
*                                                                               
*                                                                               
         XC    TEMP,TEMP                    QUANTITY RCVD=0                     
         XC    TEMP1,TEMP1                  TOTAL WCOST=0                       
         XC    TEMP2,TEMP2                  TOTAL RETAIL COST=0                 
*                                                                               
         USING BOOKD,R4                                                         
         LA    R4,TABLE                                                         
         SR    R7,R7                        ZERO OUT REC CNTR                   
LOOP     DS    0H                                                               
         LA    R9,CARD                                                          
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    SORT                    IF END OF CARD DO                        
*                                      GRAND TOTALS FOR RCDVD                   
*                                      QTY,TOTAL WHOLESALECOST                  
*                                      & TOTAL RETAIL COST                      
*                                                                               
         LA    R7,1(R7)                INCREMENT COUNTER                        
         MVC   BAUTHOR,AUTHOR                  READ FROM CARD INTO              
         MVC   BTITLE,TTLE                     TABLE.                           
         MVC   BID,IDCODE                                                       
         MVC   BTYPE,TYPE                                                       
         PACK  PACKQT,WSALE                                                     
         CVB   R3,PACKQT                                                        
         STCM  R3,3,BCOST                                                       
         PACK  PACKQT,QTY                                                       
         CVB   R3,PACKQT                                                        
         STC   R3,BQTY                                                          
         LA    R4,TABLELQ(R4)                                                   
         B     LOOP                                                             
*                                                                               
SORT     DS    0H                                                               
         LA    R1,DMCB                                                          
         LA    RE,TABLE                                                         
         ST    RE,0(R1)                                                         
         MVI   0(R1),X'00'                                                      
         ST    R7,4(R1)                                                         
         LA    RE,TABLELQ                                                       
         ST    RE,8(R1)                                                         
         LA    RE,L'BTITLE                                                      
         ST    RE,12(R1)                                                        
         LA    RE,25                                                            
         ST    RE,16(R1)                                                        
         L     RF,=V(XSORT)                                                     
         BASR  RE,RF                                                            
*                                                                               
         LA    RE,L'BAUTHOR                                                     
         ST    RE,12(R1)                                                        
         LA    RE,0                                                             
         ST    RE,16(R1)                                                        
         L     RF,=V(XSORT)                                                     
         BASR  RE,RF                                                            
*                                                                               
         LA    RE,L'BTYPE                                                       
         ST    RE,12(R1)                                                        
         LA    RE,56                                                            
         ST    RE,16(R1)                                                        
         L     RF,=V(XSORT)                                                     
         BASR  RE,RF                                                            
*                                                                               
PRINT    DS    0H                                                               
         LA    R4,TABLE                                                         
*                                                                               
PRNTLOOP DS    0H                                                               
         MVC   PRTTYPE,BTYPE                PRINT FROM TABLE                    
         MVC   PRTAUT,BAUTHOR                                                   
         MVC   PRTTITLE,BTITLE                                                  
         MVC   PRTID,BID                                                        
         PACK  PACKQT,BQTY                                                      
         ZIC   R3,BQTY                                                          
         EDIT  (R3),PRTQRCD                                                     
         SR    R8,R8                                                            
         ICM   R8,3,BCOST           CONV. WHOLE SALE COST                       
         EDIT  (R8),PRTWC,2,FLOAT=$                                             
         A     R3,TEMP             ADD QUANTITY TO TOT.QUANTITY                 
         ST    R3,TEMP             STORE TOTAL QUANTITY                         
         ZIC   R3,BQTY                                                          
         MR    R2,R8               GETTING TOTAL WCOST                          
         ST    R3,SALE             STORE TOTAL WCOST                            
         EDIT  (R3),PRTTWC,2,COMMAS=YES,FLOAT=$                                 
*        CVD   R3,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  PRTTWC,DUB                                                       
         A     R3,TEMP1            ADD TOTAL WCOST                              
         ST    R3,TEMP1            STORE TOTAL WCOST                            
         L     R3,SALE             GET TOTAL WCOST                              
*                                                                               
*GETTING TOTAL RCOST BY MULTIPLYING BY 3 AND DIVIDE BY 2(1.5)                   
*                                                                               
         M     R2,=F'3'            GETTING TOTAL RCOST                          
         SR    R2,R2                                                            
         D     R2,=F'2'                                                         
         EDIT  (R3),PRTTRC,2,COMMAS=YES,FLOAT=$                                 
         A     R3,TEMP2    ADD GRAND TOTAL RCOST TO TOTAL RCOST                 
         ST    R3,TEMP2    STORE GRAND TOTAL RCOST                              
         GOTO1 =V(PRINTER)                                                      
         LA    R4,TABLELQ(R4)                                                   
         BCT   R7,PRNTLOOP                                                      
*                                                                               
GRAND    DS    0H                                                               
         L     R5,TEMP                 GET GRAND TOTAL QTY RCVD                 
         EDIT  (R5),PRTQRCD,COMMAS=YES                                          
         L     R5,TEMP1                   GET GRAND TOTAL WCOST                 
         EDIT  (R5),PRTTWC,2,COMMAS=YES,FLOAT=$                                 
         L     R5,TEMP2                   GET GRAND TOTAL RCOST                 
         EDIT  (R5),PRTTRC,2,COMMAS=YES,FLOAT=$                                 
         GOTO1 =V(PRINTER)                                                      
*                                                                               
* THE XBASE MACRO ENDS PROGRAM EXECUTION                                        
*                                                                               
*        DS    0H                                                               
         XBASE                                                                  
         EJECT                                                                  
******************************************************                          
* IF YOU WISH TO DEFINE ANY STORAGE OR CONSTANTS,                               
* JUST APPEND THEM TO THE LIST ABOVE (PUT THEM HERE)                            
******************************************************                          
DMCB     DS    6F                                                               
CARD     DS    0CL80                                                            
TTLE     DS    CL25                                                             
AUTHOR   DS    CL25                                                             
BLANK    DS    CL4                                                              
IDCODE   DS    CL4                                                              
TYPE     DS    CL1                                                              
WSALE    DS    CL4                                                              
QTY      DS    CL3                                                              
BLANK1   DS    CL14                                                             
*                                                                               
TABLE    DS    50CL58                                                           
*                                                                               
PACKQT   DS    D                                                                
PACKWS   DS    D                                                                
TEMP     DS    D                                                                
TEMP1    DS    D                                                                
TEMP2    DS    D                                                                
SALE     DS    D                                                                
DUB      DS    D                                                                
         LTORG                                                                  
         SPACE 3                                                                
*        LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         ORG   P                                                                
PRTTYPE  DS    CL1                                                              
         DS    CL2                                                              
PRTAUT   DS    CL25                                                             
         DS    CL2                                                              
PRTTITLE DS    CL25                                                             
         DS    CL2                                                              
PRTID    DS    CL4                                                              
         DS    CL2                                                              
PRTQRCD  DS    CL3                                                              
         DS    CL2                                                              
PRTWC    DS    CL6                                                              
         DS    CL2                                                              
PRTTWC   DS    CL10                                                             
         DS    CL2                                                              
PRTTRC   DS    CL10                                                             
         ORG                                                                    
WORK     DS    CL17                                                             
BOOKD    DSECT                                                                  
BAUTHOR  DS    CL25                                                             
BTITLE   DS    CL25                                                             
BID      DS    CL4                                                              
BCOST    DS    XL2                                                              
BTYPE    DS    C                                                                
BQTY     DS    X                                                                
TABLELQ  EQU   *-BOOKD                                                          
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'179DDTRBOOK  09/08/98'                                      
         END                                                                    
