*          DATA SET NEWRIB3    AT LEVEL 008 AS OF 02/20/13                      
*PHASE T320B3A                                                                  
         TITLE 'T320B3 - WORK AREA FOR NEWRI20'                                 
NEWRWRK  CSECT                                                                  
         SPACE 1                                                                
         DC    CL8'*NETLIST'                                                    
NETLIST  DC    6000X'00'                                                        
         DC    CL8'*STALIST'                                                    
STALIST  DC    3000X'00'                                                        
         DC    CL8'GRPPRODS'                                                    
GRPPLIST DC    4500X'00'          3 PRD GRPS X 1500(3X500PRODS)                 
         DC    CL8'**UCOM**'                                                    
UCOMTBL  DC    500X'00'           NEED ABOUT 450 THIS GIVES SLACK               
         DC    CL8'*NINVTBL'                                                    
NINVTBL  DC    200X'00'           THERE IS SOME SLACK HERE                      
         DC    CL8'*TIMTAL*'      TIME+TALENT DOLLARS                           
TIMTALTB DC    2814X'00'          TABLE BUILT IN NEWRI20                        
*                                 ROOM FOR 200 NETWORKS                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008NEWRIB3   02/20/13'                                      
         END                                                                    
