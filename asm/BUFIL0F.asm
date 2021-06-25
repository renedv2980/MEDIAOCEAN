*          DATA SET BUFIL0F    AT LEVEL 016 AS OF 05/01/02                      
*PHASE T5020FA                                                                  
         TITLE 'T5020F - TEST REPORT FOR ABC DRIVER'                            
T5020F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TEST**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,REPMOD                                                        
         B     XIT                                                              
         EJECT                                                                  
*******************************************************                         
*              REPORT MODULE                                                    
*******************************************************                         
         SPACE 3                                                                
REPMOD   NTR1                                                                   
*              INITIALIZE FOR DRIVER                                            
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9031EB7'  LOAD T31EB7 (GLOBAL STORAGE)          
         L     R6,DMCB                                                          
         USING GLOBALD,R6                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A40'  LOAD T00A40 (DRIVER)                  
         MVC   DRIVER,DMCB                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A61'  LOAD T00A61 (ABC DRIVER)              
         MVC   GLASYSDR,DMCB                                                    
         GOTO1 CALLOV,DMCB,0,X'D905021F'  LOAD T5021F (DPG PHASE)               
         MVC   GLAPROG,DMCB                                                     
*                                                                               
         LA    R2,HOOK                                                          
         ST    R2,GLAHOOK                                                       
*                                                                               
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         MVI   GLLHEADL,11                                                      
         SPACE 1                                                                
         LA    R2,BUILDPL          A(BUILDER BLOCKS)                            
         ST    R2,GLABCBPL         PASSED TO DRIVER                             
         MVI   GLABCSW,1           AND TURN ON ABC SWITCH                       
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              BUILDER BLOCKS                                                   
         SPACE 3                                                                
BUILDPL  DC    A(0)                NODIO                                        
         DC    A(DTYPTAB)          DATA TYPE TABLE                              
         DC    A(OUTTAB2)          OUTLINE TABLE                                
         DC    A(ACCDEF)           ACCUMULATOR DEFINITION                       
         DC    A(ACCUMS)           ACCUMULATORS                                 
         SPACE 1                                                                
DTYPTAB  DS    0F                  DATA TYPE TABLE                              
         DC    CL19'BUDGET'                                                     
         DC    CL19'ORDER'                                                      
         DC    X'00'                                                            
         SPACE 1                                                                
OUTTAB   DS    0F                  OUTLINE TABLE                                
         DC    X'0001',12X'00',X'00010000800000',CL20'TOTAL PLAN'               
         DC    X'0002',12X'00',X'01000010800001',CL20'TELEVISION'               
         DC    X'0003',12X'00',X'02000100800002',CL20'SPOT'                     
         DC    X'0004',12X'00',X'03000000400003',CL20'NEW YORK'                 
         DC    X'0005',12X'00',X'03000000400003',CL20'LOS ANGELES'              
         DC    X'0006',12X'00',X'03000000400003',CL20'OTHER MARKETS'            
         DC    X'0007',12X'00',X'02000100400002',CL20'CABLE'                    
         DC    X'0008',12X'00',X'01000000800001',CL20'MAGAZINES'                
         DC    X'0009',12X'00',X'02000000400008',CL20'LIFE'                     
         DC    X'000A',12X'00',X'02000000400008',CL20'COSMOPOLITAN'             
         DC    X'0000'                                                          
         SPACE 1                                                                
OUTTAB2  DS    0F                  OUTLINE TABLE                                
         DC    X'0001',12X'00',X'00010000800000',CL20'TOTAL PLAN'               
         DC    X'0002',12X'00',X'01000010800001',CL20'TELEVISION'               
         DC    X'0003',12X'00',X'02000100400002',CL20'SPOT'                     
         DC    X'0004',12X'00',X'03000040400003',CL20'NEW YORK'                 
         DC    X'0005',12X'00',X'03000040400003',CL20'LOS ANGELES'              
         DC    X'0006',12X'00',X'03000040400003',CL20'OTHER MARKETS'            
         DC    X'0007',12X'00',X'02000100400002',CL20'CABLE'                    
         DC    X'0008',12X'00',X'01000000800001',CL20'MAGAZINES'                
         DC    X'0009',12X'00',X'02000000400008',CL20'LIFE'                     
         DC    X'000A',12X'00',X'02000000400008',CL20'COSMOPOLITAN'             
         DC    X'0000'                                                          
         SPACE 1                                                                
ACCDEF   DS    0F                  ACCUMULATOR DEFINITION                       
         DC    H'2'                                                             
         DC    H'16'                                                            
         DC    H'2'                                                             
         DC    H'16'                                                            
         DC    H'10'                                                            
         DC    X'00'                                                            
         SPACE 1                                                                
ACCUMS   DS    0F                  ACCUMULATORS                                 
         DC    PL8'0450000',PL8'0449000' TOTAL PLAN                             
         DC    PL8'0340000',PL8'0336000'    TELEVISION                          
         DC    PL8'0250000',PL8'0244000'       SPOT                             
         DC    PL8'0125000',PL8'0107000'           NEW YORK                     
         DC    PL8'0075000',PL8'0083000'           LOS ANGELES                  
         DC    PL8'0050000',PL8'0054000'           OTHER MARKETS                
         DC    PL8'0090000',PL8'0092000'       CABLE                            
         DC    PL8'0110000',PL8'0113000'    MAGAZINES                           
         DC    PL8'0070000',PL8'0067500'       LIFE                             
         DC    PL8'0040000',PL8'0045500'       COSMOPOLITAN                     
         DC    X'00'                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'ACCOUNT BUDGET SYSTEM'                                    
         SSPEC H1,50,C'TESTING ABC DRIVER'                                      
         SSPEC H2,50,C'------------------'                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,99,PAGE                                                       
         SSPEC H6,1,C'PLAN'                                                     
         DC    X'00'                                                            
         EJECT                                                                  
*              HEADLINE ROUTINES, ETC                                           
*                                                                               
HOOK     NTR1                              HEAD HOOK                            
*                                                                               
         CLI   GLHOOK,GLHEAD                                                    
         BNE   HKXIT                                                            
         MVC   H4+10(20),=C'DONOVAN DATA SYSTEMS'                               
         MVC   H5+10(20),=C'ABC SYSTEM          '                               
         MVC   H6+10(20),=C'CAMPAIGN FOR 1986   '                               
*                                                                               
HKXIT    DS    0H                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              STORAGE  LTORG ETC                                               
         SPACE 3                                                                
RELO     DS    A                                                                
DRIVER   DS    A                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016BUFIL0F   05/01/02'                                      
         END                                                                    
