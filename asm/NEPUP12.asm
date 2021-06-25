*          DATA SET NEPUP12    AT LEVEL 002 AS OF 08/10/00                      
*          DATA SET NEPUP12    AT LEVEL 011 AS OF 02/16/88                      
*PHASE T32212A                                                                  
         TITLE 'T32212 - PROGRAM LIST'                                          
T32212   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32212**,RA                                                    
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
         MVC   GENCKEY,KEY         SAVE GENCON'S KEY                            
         LA    R4,KEY                                                           
         USING NPUKEY,R4                                                        
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT (REQUIRED)                            
         GOTO1 VVALCLT                                                          
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         OI    PUPTITLH+6,X'80'    TITLES                                       
         MVC   PUPTITL,QTITLES     QUARTERLY                                    
         CLI   PLANPERT,C'Q'                                                    
         BE    TITEND                                                           
         MVC   PUPTITL,MTITLES     MONTHLY/WEEKLY                               
         SPACE 1                                                                
TITEND   MVC   KEY,GENCKEY         RESTORE PREVIOUS GENCON KEY                  
         MVI   NPUKTYPE,X'22'      FILL PROGRAM KEY                             
         MVC   NPUKAM,BINAGYMD                                                  
         MVC   NPUKCLT,CLTCOMP                                                  
         MVC   NPUKNET,NETWORK                                                  
         MVC   NPUKDPT,DPTCODE                                                  
         MVC   NPUKPLAN,PLANCODE                                                
         EJECT                                                                  
*              HANDLE I/O FOR LIST                                              
         SPACE 3                                                                
LIST2    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   KEY(13),KEYSAVE     MUST HAVE MATCH ON PLAN                      
         BNE   XIT                                                              
         MVC   LISTAR,SPACES                                                    
         GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
         MVC   LISTAR(6),PROGCODE                                               
         MVC   LISTAR+7(16),PROGNAME                                            
         LA    R2,PLANPLST                                                      
         ZIC   R4,PLANNPER                                                      
         XC    UNITACCS,UNITACCS                                                
         SPACE 1                                                                
LIST8    MVC   PERIOD,0(R2)        SET UP PERIOD FOR EXTUNS                     
         GOTO1 VEXTUNS                                                          
         ZIC   R3,3(R2)            RELATIVE MONTH                               
         CLI   PLANPERT,C'Q'                                                    
         BNE   *+8                                                              
         IC    R3,2(R2)            OR QUARTER                                   
         SLL   R3,2                                                             
         LA    R3,UNITACCS(R3)                                                  
         ZIC   R1,UNITS                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         BCT   R4,LIST8                                                         
         SPACE 1                                                                
         LA    R2,UNITACCS                                                      
         LA    R3,LISTAR+24                                                     
         LA    R4,13                                                            
         CLI   PLANPERT,C'Q'                                                    
         BNE   *+8                                                              
         LA    R4,4                                                             
         SR    R5,R5                                                            
         SPACE 1                                                                
LIST9    A     R5,0(R2)                                                         
         EDIT  (4,0(R2)),(3,0(R3))                                              
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,LIST9                                                         
         CLI   PLANPERT,C'Q'       SHOW TOTAL AS WELL FOR QUARTERLY             
         BNE   LIST10                                                           
         EDIT  (R5),(3,2(R3))                                                   
         SPACE 1                                                                
LIST10   GOTO1 LISTMON                                                          
         B     LIST4                                                            
         SPACE 1                                                                
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
         SPACE 1                                                                
QTITLES  DC    CL51' Q4  Q1  Q2  Q3  TOTAL'                                     
MTITLES  DC    C'SEP OCT NOV DEC JAN FEB MAR APR MAY JUN JUL AUG SEP'           
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPE2D                                                       
         SPACE 1                                                                
GENCKEY  DS    CL32                                                             
         DS    0F                                                               
UNITACCS DS    CL52                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEPUP12   08/10/00'                                      
         END                                                                    
