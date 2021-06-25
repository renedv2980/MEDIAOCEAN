*          DATA SET ACREP7602  AT LEVEL 003 AS OF 08/16/00                      
*PHASE AC7602A                                                                  
*INCLUDE CONFUSE                                                                
         TITLE 'AC76 - FILTER VALUES LISTING'                                   
AC7602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC7602,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA     RA=A(GLOBAL WORK)                                 
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(TEMP WORK)                              
         ST    R5,RELO                                                          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   FL2                                                              
         L     RE,=V(CONFUSE)                                                   
         A     RE,RELO                                                          
         ST    RE,VCONFUSE                                                      
         B     EXIT                                                             
*                                                                               
FL2      CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVC   PAGE,=H'1'          SET-UP FOR NEW REQUEST                       
         MVI   CONSW,C'F'                                                       
         XC    CONKEY,CONKEY                                                    
         MVI   CONKEY,C'F'         BUILD CONFUSE KEY                            
         MVI   CONKEY+1,C'A'                                                    
         MVC   CONKEY+2(1),RCCOMPFL                                             
         B     FL4                                                              
         EJECT                                                                  
*                                                                               
FL4      GOTO1 VCONFUSE,DMCB,(CONSW,CONKEY),CONIO,DATAMGR                       
         MVI   CONSW,C'S'          SEQ FOR NEXT CALL                            
         CLI   8(R1),0                                                          
         BE    FL6                                                              
         TM    8(R1),X'80'         E-O-F                                        
         BO    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
FL6      LA    R2,CONIO                                                         
         USING CTVREC,R2                                                        
         MVC   P+3(2),CTVKKEY+1    FORMAT KEY VALUES                            
         EDIT  (B1,CTVKNUM),(2,P+13)                                            
         LA    R3,CTVDATA                                                       
FL8      CLI   0(R3),0                                                          
         BE    FL20                                                             
         CLI   0(R3),X'A0'         DESCRIPTION ELEMENT                          
         BE    FL12                                                             
         CLI   0(R3),X'A1'         VALUE ELEMENT                                
         BE    FL14                                                             
FL10     ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     FL8                                                              
         EJECT                                                                  
*                                  HANDLE DEFINITION ELEMENT                    
FL12     ZIC   R1,1(R3)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     FL10                                                             
         MVC   P+21(0),2(R3)                                                    
*                                  HANDLE VALUE ELEMENT                         
         USING CTVVLD,R3                                                        
FL14     ZIC   R1,CTVVLLEN                                                      
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+50(0),CTVVLNAM                                                 
         MVC   P+41(1),CTVVLCHR                                                 
         CLI   CTVVLTYP,C'A'                                                    
         BE    FL18                                                             
         CLI   CTVVLTYP,C'N'                                                    
         BNE   FL16                                                             
         MVC   P+41(2),=C'N='                                                   
         EDIT  (B1,CTVVLCHR),(3,P+43),ALIGN=LEFT                                
         B     FL18                                                             
FL16     MVC   P+41(2),=C'H='                                                   
         GOTO1 HEXOUT,DMCB,CTVVLCHR,P+43,1,=C'TOG'                              
FL18     GOTO1 ACREPORT                                                         
         B     FL10                                                             
*                                                                               
FL20     GOTO1 ACREPORT                                                         
         B     FL4                                                              
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    F                                                                
VCONFUSE DS    V                                                                
CONKEY   DS    CL4                                                              
CONSW    DS    C                                                                
CONIO    DS    1000C                                                            
*                                                                               
* ACREPWORKD/ACGENMODES/CTGENFILE                                               
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREP7602 08/16/00'                                      
         END                                                                    
