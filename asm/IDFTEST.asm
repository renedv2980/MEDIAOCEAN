*          DATA SET IDFTEST    AT LEVEL 001 AS OF 12/06/99                      
*PHASE IDFTEST                                                                  
*INCLUDE LOADER                                                                 
         TITLE 'MODULE TO TEST ASMIDF DEBUGGER'                                 
         PRINT NOGEN                                                            
IDFTEST  CSECT                                                                  
         NBASE WORKL,**IDFTST,=A(WORKAREA),CLEAR=YES                            
         USING WORKD,RC                                                         
         ENTRY SSB                                                              
         ENTRY MASTC                                                            
         SPACE 2                                                                
         LA    R4,SYSPRINT                                                      
         ST    R4,ADCB                                                          
         OPEN  ((R4),OUTPUT)                                                    
         MVI   PLINE,C' '                                                       
         MVC   PLINE+1(L'PLINE-1),PLINE+0                                       
         MVC   PLINE+1(20),MESSAGE                                              
         MVC   DATEOUT,DATE                                                     
         MVC   PLINE+30(L'DATEOUT),DATEOUT                                      
         PUT   SYSPRINT,PLINE                                                   
         BAS   RE,SUB                                                           
         SPACE 2                                                                
* LOAD IN A CHILD PROGRAM                                                       
*                                                                               
         GOTO1 LOADER,DMCB,PGMNAME,0,0                                          
         MVC   LENPGM,DMCB+0                                                    
         MVC   APGM,DMCB+4                                                      
*                                                                               
* CALL THE CHILD PROGRAM                                                        
*                                                                               
         GOTO1 APGM,DMCB,WORKD                                                  
         SPACE 2                                                                
         CLOSE SYSPRINT                                                         
         XBASE RC=0                                                             
         SPACE 2                                                                
SUB      NTR1  ,                                                                
         MVI   PLINE+0,C' '                                                     
         MVC   PLINE+1(L'PLINE-1),PLINE+0                                       
         PUT   SYSPRINT,PLINE                                                   
         MVC   PLINE(L'SECTXT),SECTXT                                           
         PUT   SYSPRINT,PLINE                                                   
SUBX     XIT1                                                                   
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
LOADER   DC    V(LOADER)                                                        
LENPGM   DC    A(0)                                                             
APGM     DC    A(0)                                                             
SYSPRINT DCB   DDNAME=SYSPRINT,MACRF=(PM),DSORG=PS,RECFM=FBM,LRECL=133          
DATE     DC    C'991123'                                                        
MESSAGE  DC    CL20'TEST ASMIDF PROGRAM'                                        
SECTXT   DC    CL40'A SECOND LINE OF TEXT TO PRINT'                             
PGMNAME  DC    CL8'IDFPGM'                                                      
WORKAREA DC    8000X'00'                                                        
         DS    0D                                                               
SSB      DC    X'00000000'                                                      
         DC    XL250'00'                                                        
         DS    0D                                                               
MASTC    DS    XL2048'00'                                                       
         DS    0D                                                               
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
DMCB     DS    6F                                                               
ADCB     DS    A                                                                
DATEOUT  DS    CL6                                                              
PLINE    DS    CL133                                                            
WORKL    EQU   *-WORKD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001IDFTEST   12/06/99'                                      
         END                                                                    
