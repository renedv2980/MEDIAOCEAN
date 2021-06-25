*          DATA SET REDARPD    AT LEVEL 006 AS OF 08/17/15                      
*PHASE REDARPDA                                                                 
**********************************************************************          
* RE/DAR DUPLICATE PASSIVE KEYS LIST.                                           
* THIS IS LOADED IN REDARPAS TO ELMINATE ANY DUPLICATE PASSIVE KEYS             
* DUE TO X'51' AND X'41' RECORDS CONFLICT.                                      
* X'51' CREATED AS SOMEONE CONFIRMED THE ORDER.                                 
* X'41' CREATED WHEN SOMEONE REVISED THE ORDER AFTER CONFIRMED.                 
**********************************************************************          
REDARPD  CSECT                                                                  
         DS    0C                                                               
DUPES  DC XL27'A01100000000000000D7E5D9C1D1E340D2C2E9D24005643379C1C1'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
DUPESL   EQU   *-DUPES                                                          
       DC XL27'A01100000000000000D7E5D9C1D1E340D2C2E9D24005643379C1C2'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'A11100000000000000D7E5E340D9C1D1D2C2E9D24005643379C1C1'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'A11100000000000000D7E5E340D9C1D1D2C2E9D24005643379C1C2'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102D8C7E2D140D5E8C3D9C5404040E6C8C9400000000162220003'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102D8C7E2D140D5E8E3D4F2404040C1D5C3400000000151370054'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102E2D1E2D140D5E8C3D9C5404040E6C8C9400000000162280011'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102E2D1E2D140D5E8D1D4C7404040C7D3D640000000AF62340001'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102E2D1E7C1E2C1E3C7E3C3404040C7E3D7400000000000119186'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
***                                                                             
       DC XL27'D102E2E9C4C9D5C1E3F3C8C6F14040C3D6D440000001FB00506965'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102E2E9C4E4D4D5E8F1C3C4D64040E2C540400000036B00485305'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102E2E9C7D4C1D5E8E4D4C8404040C4C1D940000000B804141649'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102E2E9C7D4D4C1E3C1C8C3404040E3E340400000001504143751'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102E2E9C7D4E4C1E3D2C6E4404040C4C5D7400000000E04142829'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
       DC XL27'D102C1D4E3D9C7C4C1C2D3E3404040F0F0404000004A8600357903'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
***                                                                             
       DC XL27'D102E2E9D4C4E7D5E8C4C7E7404040C3D6D9400000001404149315'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
***                                                                             
       DC XL27'D102C1D4D7C5D9D4D5C7C3C9404040C7C3C94000044BEA00022846'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
***                                                                             
       DC XL27'D10CC1D4E68AD7C5D9D4D500022846000000000000000000000000'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
***                                                                             
       DC XL27'D102E2E9C7E240C4C1D7E8C3404040C7C5D5400000001004003623'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
***                                                                             
       DC XL27'D102E2E9C7E240C4C1D7E8C3404040C7C5D5400000001104004160'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
***                                                                             
       DC XL27'D102E2E9D4E9D4D5E8C3C8C1404040C3C8D2400000002501315060'          
         DC    XL5'00'                                                          
         DC    XL32'00'                                                         
         DC    XL32'00'                                                         
***                                                                             
         DC    96X'FF'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006REDARPD   08/17/15'                                      
         END                                                                    
