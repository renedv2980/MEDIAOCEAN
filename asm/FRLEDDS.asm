*          DATA SET FRLEDDS    AT LEVEL 001 AS OF 10/26/99                      
*PHASE LEDDS                                                                    
*INCLUDE DATCON                                                                 
         TITLE 'SUB-TASK MODULE TO RECEIVE LE MAIN MODULE CALL'                 
         PRINT NOGEN                                                            
LEDDS    RMODE 24                                                               
LEDDS    AMODE 24                                                               
LEDDS    CSECT                                                                  
         NBASE WORKL,**LEDDS*,=A(WORKAREA),CLEAR=YES                            
         USING WORKD,RC                                                         
         ENTRY SSB                                                              
         ENTRY MASTC                                                            
         SPACE 2                                                                
         LR    R5,RC                                                            
         AHI   R5,-4                                                            
         L     R5,0(R5)            A(R1 PARAMETER FROM LINK)                    
         SPACE 2                                                                
         LA    R4,SYSOUT                                                        
         OPEN  ((R4),OUTPUT)                                                    
         MVC   STRING(20),0(R5)    SAVE PIECE OF STRING IN STORAGE              
         MVI   PLINE,C' '                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE+0                                       
         MVC   PLINE+1(20),STRING                                               
         MVC   DATE,=X'991020'                                                  
         GOTO1 DATCON,DMCB,(1,DATE),(0,DATEOUT)                                 
         WTO   'AFTER DATCON CALL',MCSFLAG=HRDCPY                               
         MVC   PLINE+30(L'DATEOUT),DATEOUT                                      
         PUT   SYSOUT,PLINE                                                     
         CLOSE SYSOUT                                                           
         XBASE RC=0                                                             
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
DATCON   DC    V(DATCON)                                                        
SYSOUT   DCB   DDNAME=SYSOUT,MACRF=(PM),DSORG=PS,RECFM=FBM,LRECL=133            
WORKAREA DC    8000X'00'                                                        
SSB      DC    X'00000000'                                                      
         DC    XL250'00'                                                        
MASTC    DS    100D                                                             
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DMCB     DS    6F                                                               
DATE     DS    XL3                                                              
DATEOUT  DS    CL6                                                              
PLINE    DS    CL133                                                            
STRING   DS    CL80                                                             
WORKL    EQU   *-WORKD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001FRLEDDS   10/26/99'                                      
         END   LEDDS                                                            
