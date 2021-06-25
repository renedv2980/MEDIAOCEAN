*          DATA SET ACREPZY02  AT LEVEL 145 AS OF 04/05/13                      
*PHASE ACZY02C                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*                                                                               
ACZY02   TITLE 'POPULATE ABLELD/ABLTXS WITH # OF TRANSACTIONS'                  
*               USE ACAJTST JCL                                                 
*                                                                               
* ACLDXABL MUST BE RUN FIRST BECAUSE IT ADDS/UPDATES THE ABLEL TO THE           
* CORRECT LENGTH                                                                
* THIS PROGRAM ASSUMES IT IS THE CORRECT LENGTH                                 
*                                                                               
ACZY02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ZY02**,CLEAR=Y                                               
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          GLOBAL STORAGE                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            LOCAL STORAGE(SAVED STORAGE)                 
         LARL  R9,GLOBALS                                                       
         USING GLOBALS,R9          RA=A(GLOBAL LITERALS)                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
         JE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         JE    RUNL                                                             
         CLI   MODE,COMPFRST                                                    
         JE    COMF                                                             
         CLI   MODE,PROCACC                                                     
         JE    PROC                                                             
         CLI   MODE,PROCTRNS                                                    
         JE    PRCT                                                             
         CLI   MODE,ACCLAST                                                     
         JE    ACCL                                                             
         CLI   MODE,COMPLAST                                                    
         JE    COML                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* RUNFRST                                                             *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         J     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR COMPANY                                                   *         
***********************************************************************         
         SPACE 1                                                                
COMF     DS    0H                                                               
         XC    TOTALC,TOTALC                                                    
         XC    COUNTOT,COUNTOT                                                  
         J     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PROC     DS    0H                                                               
         XC    COUNTER,COUNTER                                                  
         J     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
PRCT     L     R2,ADTRANS                                                       
         USING TRNELD,R2                                                        
         LA    R3,TRNELD                                                        
         SH    R3,DATADISP                                                      
         USING TRNRECD,R3                                                       
         TM    TRNKSTA,TRNSDRFT                                                 
         JNZ   EXIT                                                             
         ICM   RE,15,COUNTER                                                    
         AHI   RE,1                                                             
         STCM  RE,15,COUNTER                                                    
         J     EXIT                                                             
         DROP  R2,R3                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR ACCOUNT                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
ACCL     L     R3,ADACC                                                         
         OC    ADACCBAL,ADACCBAL   R2=A(BALANCE ELEMENT)                        
         JNZ   ACCL02                                                           
         MVC   P+1(8),=C'COMPANY '                                              
         XOUT  ACTKCPY,P+9,1                                                    
         MVC   P+15(14),ACTKULA                                                 
         MVC   P+30(12),=C'TOTAL # TRNX'                                        
         XOUT  COUNTER,P+46,4                                                   
         MVC   P+60(14),=C'MISSING ABLELD'                                      
         GOTO1 ACREPORT                                                         
         J     EXIT                                                             
                                                                                
         USING ABLELD,R2                                                        
ACCL02   L     R2,ADACCBAL         R2=A(BALANCE ELEMENT)                        
         OC    COUNTER,COUNTER                                                  
         JZ    *+10                                                             
         MVC   ABLTXS,COUNTER      POPULATE ABLTXS                              
                                                                                
         MVC   P+1(8),=C'COMPANY '                                              
         XOUT  ACTKCPY,P+9,1                                                    
         MVC   P+15(14),ACTKULA                                                 
         MVC   P+30(12),=C'TOTAL # TRNX'                                        
         EDIT  (B4,COUNTER),(6,P+46)                                            
*        XOUT  COUNTER,P+46,4                                                   
         GOTO1 ACREPORT                                                         
         XOUT  ABLELD,P+1,40                                                    
         GOTO1 ACREPORT                                                         
         DROP  R3                                                               
                                                                                
         XR    RE,RE                                                            
         ICM   RE,15,COUNTOT                                                    
         A     RE,COUNTER                                                       
         STCM  RE,15,COUNTOT                                                    
                                                                                
         XC    COUNTER,COUNTER                                                  
                                                                                
         XR    RE,RE                                                            
         ICM   RE,15,TOTALC                                                     
         AHI   RE,1                                                             
         STCM  RE,15,TOTALC                                                     
                                                                                
         CLI   RCWRITE,C'N'                                                     
         JE    EXIT                                                             
                                                                                
         MVI   MODE,WRITACC                                                     
         J     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RUN                                                                  
***********************************************************************         
         SPACE 1                                                                
COML     DS    0H                                                               
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         MVC   P+1(8),=C'COMPANY '                                              
         XOUT  ACTKCPY,P+9,1                                                    
         MVC   P+15(16),=C'TOTAL # OF A/CS '                                    
         EDIT  (B4,TOTALC),(6,P+36)                                             
*        XOUT  TOTALC,P+36,4     TOTAL NUMBER OF ACCOUNTS COUNTED               
         MVC   P+50(18),=C'TOTAL # OF RECORDS'                                  
         EDIT  (B4,COUNTOT),(6,P+72)                                            
*        XOUT  COUNTOT,P+72,4                                                   
         GOTO1 ACREPORT                                                         
         J     EXIT                                                             
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RUN                                                                  
***********************************************************************         
         SPACE 1                                                                
RUNL     DS    0H                                                               
         J     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         SPACE 1                                                                
***********************************************************************         
WORKD    DSECT                                                                  
COUNTER  DS    XL4                                                              
TOTALC   DS    XL4                                                              
COUNTOT  DS    XL4                                                              
WORKX    EQU   *-WORKD                                                          
         EJECT                                                                  
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'145ACREPZY02 04/05/13'                                      
         END                                                                    
