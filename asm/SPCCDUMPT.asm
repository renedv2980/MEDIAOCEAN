*          DATA SET SPCCDUMPT  AT LEVEL 105 AS OF 06/16/86                      
*PHASE SPGX02T,+0                                                               
SPGX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GX02**                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         OPEN  (CCINPUT,(INPUT))                                                
         LA    R5,INREC                                                         
         USING COKETAPE,R5                                                      
GETCOKE  GET   CCINPUT,INREC                                                    
         CLI   CCRECTYP,C'1'       IS IT A GOAL                                 
         BNE   GETCOKE                                                          
         CLC   CCMKT,QMKT                                                       
         BNE   GETCOKE                                                          
         MVC   P(132),INREC                                                     
         GOTO1 REPORT                                                           
         B     GETCOKE                                                          
         SPACE 2                                                                
CCEOF    CLOSE (CCINPUT,REWIND)                                                 
         SPACE 2                                                                
EXIT     DS    0H                                                               
CCXIT    XMOD1 1                                                                
         EJECT                                                                  
CCFIRST  DC    X'01'                                                            
CCPTAB   DC    C'01',C'CC '                                                     
         DC    C'02',C'FR '                                                     
         DC    C'03',C'TB '                                                     
         DC    C'04',C'SP '                                                     
         DC    C'05',C'PB '                                                     
         DC    C'06',C'MY '                                                     
         DC    C'07',C'FN '                                                     
         DC    C'09',C'RB '                                                     
         DC    C'11',C'MM '                                                     
         DC    C'12',C'DC '                                                     
         DC    C'13',C'CY '                                                     
         DC    X'FF'                                                            
LNCCPTAB EQU   5                                                                
*                                                                               
CCADJTAB DC    C'00',X'00',C'K'    PACKAGE 3 LITER                              
         DC    C'01',X'00',C'M'    MAIN THRUST                                  
         DC    C'02',X'00',C'O'    COKE MAIN THRUST                             
         DC    C'03',X'02',C'P'    PROMOTIONAL                                  
         DC    C'04',X'03',C'Z'    MISCELLANEOUS                                
         DC    C'05',X'01',C'U'    UTC                                          
         DC    C'06',X'05',C'B'    BLACK                                        
         DC    C'07',X'06',C'S'    HISPANIC                                     
         DC    C'08',X'00',C'F'    CAFFEINE FREE                                
         DC    C'09',X'00',C'D'    DIET  DC                                     
         DC    X'FF'                                                            
LNADJTAB EQU   4                                                                
*                                                                               
*              BRAND/CATAGORY/FILM TYPE/PRODUCT CODE                            
CCSPCODE DC    C'01',C'02',C'O',C'CC '                                          
         DC    C'01',C'08',C'F',C'CF '                                          
         DC    C'01',C'01',C'M',C'CL '                                          
         DC    C'12',C'01',C'M',C'DC '                                          
         DC    C'12',C'08',C'F',C'CO '                                          
         DC    C'13',C'01',C'M',C'CY '                                          
         DC    C'13',C'09',C'D',C'DY '                                          
         DC    C'03',C'01',C'M',C'TB '                                          
         DC    C'03',C'08',C'F',C'CT '                                          
         DC    C'04',C'01',C'M',C'SP '                                          
         DC    C'04',C'09',C'D',C'DS '                                          
         DC    C'02',C'01',C'M',C'FR '                                          
         DC    C'05',C'01',C'M',C'PB '                                          
         DC    C'06',C'01',C'M',C'MY '                                          
         DC    C'07',C'01',C'M',C'FN '                                          
         DC    C'09',C'01',C'M',C'RB '                                          
         DC    C'11',C'01',C'M',C'MM '                                          
         DC    X'FF'                                                            
LNSPCODE EQU   8                                                                
*                                                                               
CCQTAB   DC    X'02',C'1'                                                       
         DC    X'05',C'2'                                                       
         DC    X'08',C'3'                                                       
         DC    X'0B',C'4'                                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
CCQTRS   DC    C'851841231850331',AL1(11),30X'00'                               
         DC    C'852850401850630',AL1(21),30X'00'                               
         DC    C'853850701850929',AL1(31),30X'00'                               
         DC    C'854850930851229',AL1(41),30X'00'                               
         DC    C'861851230860330',AL1(51),30X'00'                               
         DC    C'862860331860629',AL1(61),30X'00'                               
         DC    C'863860630860928',AL1(71),30X'00'                               
         DC    C'864860929861228',AL1(81),30X'00'                               
         DC    X'FF'                                                            
INREC    DS    CL132                                                            
ERROR    DS    CL50                                                             
INDEX    DS    C                                                                
BNCLT    DS    CL2                                                              
INTRLIST DS    CL1320                                                           
         LTORG                                                                  
         PRINT NOGEN                                                            
CCINPUT  DCB   DDNAME=CCINPUT,DSORG=PS,RECFM=FB,LRECL=132,             X        
               BLKSIZE=6204,MACRF=GM,EODAD=CCEOF                                
         EJECT                                                                  
SORTREC  DSECT                                                                  
SRSTART  DS    0C                                                               
SRCLT    DS    CL3                 CLIENT                                       
SRPRD    DS    CL3                 PRODUCT                                      
SRMKT    DS    XL2                 MARKET                                       
SREST    DS    XL1                 ESTIMATE                                     
SRDPT    DS    CL1                 DAYPART                                      
SRSLN    DS    XL1                 SPOT LENGTH                                  
SRWKOF   DS    XL2                 WEEK OF (MONDAY START DATE)                  
SRDATA   DS    0C                                                               
SRGRP    DS    XL4                 RATING POINTS                                
SRDOL    DS    XL4                 DOLLARS                                      
SRGRP2   DS    XL4                 SECONDARY GRPS                               
SRCATCC  DS    CL2                 CATAGORY CODE                                
SREND    DS    0C                                                               
SRLN     EQU   SREND-SRSTART                                                    
SRKLN    EQU   SRDATA-SRSTART                                                   
         SPACE 2                                                                
CPTABD   DSECT                                                                  
CPACLT   DS    CL3                                                              
CPAPRD   DS    CL3                                                              
CPBCLT   DS    CL2                                                              
CPBPRD   DS    CL1                                                              
CPTKLN   EQU   6                                                                
CPTABLN  EQU   9                                                                
         EJECT                                                                  
COKETAPE DSECT                     COKE GOAL TAPE                               
CCRECTYP DS    CL1                 C'1'                                         
CCBTLR   DS    CL5                                                              
CCPRD    DS    CL2                                                              
CCYEAR   DS    CL4                 YYYY                                         
CCCLT    DS    CL3                 (AGENCY)                                     
CCMKT    DS    CL4                 ADI MARKET NUMBER                            
CCQTR    DS    CL1                 QUARTER                                      
CCDPT    DS    CL1                 DAYPART                                      
CCCPP    DS    CL6                 9999.99                                      
CCWKGRP  DS    14CL4               WEEKLY GRPS (9999)                           
CCTOTGRP DS    CL5                 TOTAL GRP                                    
         DS    CL14                                                             
CCCATCC  DS    CL2                 CATEGORY CODE                                
         DS    CL28                SPARE                                        
         SPACE 2                                                                
CCQTRSD  DSECT                     COKE QUARTER TABLE                           
CCQST    DS    0C                                                               
CCQYY    DS    CL2                 YEAR                                         
CCQQTR   DS    CL1                 QUARTER                                      
CCQSTDT  DS    CL6                 START DATE                                   
CCQENDT  DS    CL6                 END DATE                                     
CCQEST   DS    X                                                                
CCQWKS   DS    15CL2               15 WEEKLY START DATES                        
CCQEN    DS    0C                                                               
CCLNQTRD EQU   CCQEN-CCQST                                                      
         EJECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
LNGDELEM EQU   GLEMENT-GDELEM      LENGTH OF DESC. ELEMENT                      
LNGLHDRR EQU   GDELEM-GDESC        LENGTH OF RECORD HEADER                      
LNGLELEM EQU   GLKELEM-GLEMENT     LENGTH OF GOAL DATA ELEMENT                  
LNGLREC  EQU   LNGLHDRR+LNGDELEM   LENGTH OF SKELETON RECORD                    
         EJECT                                                                  
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105SPCCDUMPT 06/16/86'                                      
         END                                                                    
