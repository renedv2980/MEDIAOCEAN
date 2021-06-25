*          DATA SET ACREPDJ01  AT LEVEL 003 AS OF 08/16/00                      
*PHASE ACDJ01A,*                                                                
ACDJ01   TITLE '- SPECS FOR NEW DAILY JOURNAL'                                  
* SUB-PROGRAM 0 - DAILY JOURNAL (INS/ONL + DRAFTS/OVNGHT) - ALL A/CS            
*             1 -   "      "        "      "         "    - NO SUB A/CS         
*             2 -   "      "    (SPECIAL OVERNIGHT)                             
*             3 - LEDGER SUMMARY                                                
*             4 - OFFICE SUMMARY                                                
*             5 - BATCH SUMMARY                                                 
*             6 - BATCH EXCEPTION LIST                                          
*             7 - BATCH DUE LIST                                                
*             8 - DDS SUMMARY                                                   
*                                                                               
* SET FOR LOWER CASE (RUNFRST)                                                  
*                                                                               
ACDJ01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF NOREP                                                            
         ACDEF NOSUM                                                            
*                                                                               
         ACDEF SPROG,0,1,2,4,5,6,7,8                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,REPORT                                                     
         ACDEF H1,123,PAGE                                                      
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,52,AC#DJRN,13                                                 
         ACDEF H2,52,AC#DJRN,13,LU                                              
         ACDEF H5,2,AC#USRID,7                                                  
         ACDEF H4,52,AC#OPNDA,11                                                
         ACDEF H5,52,AC#EFFDT,14                                                
         ACDEF H6,52,AC#UPDAT,12                                                
         ACDEF H4,98,AC#BATTY,10                                                
         ACDEF H5,98,AC#BATM,15                                                 
         ACDEF H6,98,AC#BATRF,15                                                
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H1,52,AC#DJRN,13                                                 
         ACDEF H2,52,AC#DJRN,13,LU                                              
         ACDEF H5,2,AC#USRID,7                                                  
         ACDEF H4,52,AC#WFILE,11                                                
         ACDEF H5,52,AC#PSTMO,11                                                
         ACDEF H4,98,AC#CRTDT,12                                                
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,89,REPORT                                                     
         ACDEF H1,106,PAGE                                                      
         ACDEF H1,46,AC#LGBTS,25                                    ^HO         
         ACDEF H2,46,AC#LGBTS,25,LU                                 ^HO         
         ACDEF H1,46,AC#LGBTS,29                                     HO         
         ACDEF H2,46,AC#LGBTS,29,LU                                  HO         
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H1,43,AC#APSMO,39                                                
         ACDEF H2,43,AC#APSMO,39,LU                                             
*                                                                               
         ACDEF SPROG,5                                                          
         ACDEF H1,50,AC#BATSR,20                                                
         ACDEF H2,50,AC#BATSR,20,LU                                             
*                                                                               
         ACDEF SPROG,6                                                          
         ACDEF H1,55,AC#XCNSU,17                                    ^HO         
         ACDEF H2,55,AC#XCNSU,17,LU                                 ^HO         
         ACDEF H1,55,AC#XCNSU,22                                     HO         
         ACDEF H2,55,AC#XCNSU,22,LU                                  HO         
*                                                                               
         ACDEF SPROG,7                                                          
         ACDEF H1,42,AC#BATDU,30                                                
         ACDEF H2,42,AC#BATDU,30,LU                                             
*                                                                               
         ACDEF SPROG,8                                                          
         ACDEF H1,45,AC#DDSBS,24                                                
         ACDEF H2,45,AC#DDSBS,24,LU                                             
*                                                                               
* ACDDEQUES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPDJ01 08/16/00'                                      
         END                                                                    
