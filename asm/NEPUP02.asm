*          DATA SET NEPUP02    AT LEVEL 008 AS OF 06/11/98                      
*          DATA SET NEPUP02    AT LEVEL 009 AS OF 07/26/88                      
*PHASE T32202A,*                                                                
         TITLE 'T32202 - PLAN LIST'                                             
T32202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32202**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FOR LIST                                            
         SPACE 3                                                                
         MVC   GENCKEY,KEY                                                      
         LA    R2,PUPCLIH          CLIENT (REQUIRED)                            
         GOTO1 VVALCLT                                                          
         MVC   KEY,GENCKEY                                                      
         LA    R4,KEY                                                           
         USING NPLKEY,R4                                                        
         CLI   NPLKTYPE,X'20'      ARE WE PROCESSING PLANS YET                  
         BE    LIST2                                                            
         SPACE 1                                                                
*                                  REST OF FIELDS OPTIONAL                      
         XC    NETWORK,NETWORK                                                  
         MVI   DPTCODE,0                                                        
         XC    PLANCODE,PLANCODE                                                
         LA    R2,PUPNETH          NETWORK                                      
         CLI   5(R2),0                                                          
         BE    LIST1                                                            
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         CLI   5(R2),0                                                          
         BE    LIST1                                                            
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         CLI   5(R2),0                                                          
         BE    LIST1                                                            
         GOTO1 ANY                                                              
         MVC   PLANCODE,WORK                                                    
         EJECT                                                                  
*              HANDLE I/O FOR LIST                                              
         SPACE 3                                                                
LIST1    XC    KEY,KEY                                                          
         MVI   NPLKTYPE,X'20'      FILL PLAN KEY                                
         MVC   NPLKAM,BINAGYMD                                                  
         MVC   NPLKCLT,CLTCOMP                                                  
         MVC   NPLKNET,NETWORK                                                  
         MVC   NPLKDPT,DPTCODE                                                  
         MVC   NPLKPLAN,PLANCODE                                                
         SPACE 1                                                                
LIST2    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   KEY(2),KEYSAVE      MUST HAVE MATCH ON AGENCY                    
         BNE   XIT                                                              
         CLC   KEY+2(2),CLTCOMP         AND CLIENT                              
         BNE   XIT                                                              
         CLI   PUPNETH+5,0         OPTIONAL NETWORK                             
         BE    FORMLIST                                                         
         CLC   KEY(9),KEYSAVE                                                   
         BNE   XIT                                                              
         CLI   PUPDPTH+5,0         OPTIONAL DAYPART                             
         BE    FORMLIST                                                         
         CLC   KEY(11),KEYSAVE                                                  
         BNE   XIT                                                              
         SPACE                                                                  
FORMLIST MVC   LISTAR,SPACES                                                    
         GOTO1 GETREC                                                           
         GOTO1 VEXTPLAN                                                         
         L     R4,AIO                                                           
         USING NPLRECD,R4                                                       
         LA    R2,LISTAR           DISPLAY NETWORK                              
         MVC   0(4,R2),NPLKNET                                                  
         SPACE 1                                                                
         LA    R2,5(R2)            DAYPART                                      
         MVC   WORK,NPLKDPT                                                     
         GOTO1 VLUPDPT                                                          
         MVC   0(8,R2),DPTNAME                                                  
         SPACE 1                                                                
         LA    R2,9(R2)            PLAN CODE                                    
         MVC   0(4,R2),NPLKPLAN                                                 
         SPACE 1                                                                
         LA    R2,5(R2)            PLAN NAME                                    
         MVC   0(14,R2),PLANNAME                                                
*        MVC   0(16,R2),PLANNAME                                                
         SPACE 1                                                                
         LA    R2,15(R2)           PLAN YEAR                                    
*        LA    R2,17(R2)           PLAN YEAR                                    
*        MVC   0(2,R2),=C'19'                                                   
*        EDIT  (1,PLANYEAR),(2,2(R2))                                           
         ZIC   R3,PLANYEAR                                                      
         AH    R3,=H'1900'                                                      
         EDIT  (R3),(4,0(R2))                                                   
         SPACE 1                                                                
         LA    R2,5(R2)            UNIVERSE CODE                                
         OC    PLANUNIV,PLANUNIV                                                
         BZ    FORM2                                                            
         MVC   DUB+6(2),PLANUNIV                                                
         L     R1,DUB+4                                                         
         SLL   R1,4                                                             
         ST    R1,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         EDIT  (R1),(4,0(R2)),ALIGN=LEFT                                        
         SPACE 1                                                                
FORM2    LA    R2,5(R2)            HUT YEAR                                     
         EDIT  (1,PLANHTYR),(2,0(R2))                                           
         CLI   PLANHTNO,2                                                       
         BL    FORM4                                                            
         MVI   2(R2),C','                                                       
         EDIT  (1,PLANHTNO),(1,3(R2))                                           
         SPACE 1                                                                
FORM4    MVC   5(1,R2),PLANHTSC    HUT SCHEME                                   
         MVC   6(1,R2),PLANHTAV    HUT AVERAGE                                  
*        MVC   7(1,R2),PLANHTAV    HUT AVERAGE                                  
         SPACE 1                                                                
         LA    R2,8(R2)            TARGET DEMO                                  
*        LA    R2,9(R2)            TARGET DEMO                                  
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(0,DEMOS),(10,WORK),(C'S',DBLOCK)                   
         MVC   0(10,R2),WORK                                                    
         SPACE 1                                                                
         LA    R2,10(R2)            GUARANTEED CPM                              
         EDIT  (4,GUARCPM),(6,0(R2)),2,ALIGN=LEFT,ZERO=BLANK,FLOAT=$            
         GOTO1 LISTMON                                                          
         B     LIST4                                                            
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPF2D                                                       
GENCKEY  DS    CL20                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008NEPUP02   06/11/98'                                      
         END                                                                    
