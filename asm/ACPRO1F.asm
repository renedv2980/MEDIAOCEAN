*          DATA SET ACPRO1F    AT LEVEL 001 AS OF 01/22/93                      
*PHASE T60B1FA,*                                                                
         TITLE 'T60B1F - STUDIO TYPE LIST'                                      
T60B1F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B1F**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         EJECT                                                                  
*              HANDLE I/O FOR STUDIO TYPES                                      
         SPACE 3                                                                
         LA    R4,KEY                                                           
         USING STURECD,R4                                                       
         CLI   STUKTYP,STUKTYPQ    ARE WE HANDLING STUDIO TYPES YET?            
         BNE   *+12                NO                                           
         CLI   STUKSUB,STUKSUBQ                                                 
         BE    LIST2               YES                                          
         XC    STUKEY,STUKEY                                                    
         MVI   STUKTYP,STUKTYPQ                                                 
         MVI   STUKSUB,STUKSUBQ                                                 
         MVC   STUKCPY,CUL                                                      
         MVC   STUKCODE,PROSTA                                                  
         OC    STUKCODE,SPACES                                                  
         B     LIST3                                                            
*                                                                               
LIST2    ZIC   R1,STUKCODE+3                                                    
         LA    R1,1(R1)            FORCE READ OF NEXT TYPE                      
         STC   R1,STUKCODE+3                                                    
*                                                                               
LIST3    GOTO1 HIGH                                                             
         B     LIST6                                                            
*                                                                               
LIST4    GOTO1 SEQ                                                              
*                                                                               
LIST6    CLC   KEY(3),KEYSAVE      RECORD/AGENCY CHANGE?                        
         BNE   XIT                 YES                                          
         MVC   LISTAR,SPACES                                                    
         L     R4,AIO                                                           
         LA    R3,LISTAR+1                                                      
         MVC   0(4,R3),3(R4)       SHOW TYPE                                    
         LA    R3,LISTAR+9                                                      
         GOTO1 SETNAME,DMCB,AIO,(R3)                                            
         GOTO1 LISTMON                                                          
         B     LIST4                                                            
         EJECT                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROEFD                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACPRO1F   01/22/93'                                      
         END                                                                    
