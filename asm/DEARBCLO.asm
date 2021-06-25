*          DATA SET DEARBCLO   AT LEVEL 002 AS OF 02/13/12                      
*PHASE DEARCLOA                                                                 
         TITLE 'DEMCON - RADIO CONVERSION - OUTPUT PHASE'                       
DEARBCLO CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 RADWRKX-RADWRKD,**RADCNV                                         
         USING RADWRKD,RC          RC=A(W/S)                                    
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(SORT RECORD)                            
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA         RA=A(COMMON FACILITIES)                      
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNV4                LAST TIME HOOK                               
*                                                                               
CNV2     DS    0H                                                               
*                                                                               
         LA    R6,4(R9)            SET TO SORT KEY                              
         USING DRKEY,R6                                                         
         CLI   INTRTYP,DSECDEQU    CALL-LETTER CHANGE PASSIVE RECORD            
         BE    CNV60                                                            
         B     CNVX                                                             
*                                                                               
CNV4     DS    0H                                                               
         B     CNVX                                                             
*                                                                               
CNVX     XMOD1 1                                                                
         EJECT                                                                  
*=============== STATION EQUIVALENCE RECORDS (PASSIVE) ===============*         
CNV60    LA    R6,THISKEY                                                       
         USING DSEKEY,R6                                                        
         XC    THISKEY,THISKEY                                                  
         MVC   0(L'DSEKMAJ,R6),INTKEY                                           
         GOTO1 ABLDREC,DMCB,(C'P',DSEKEY)                                       
         GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         DROP  R6                                                               
***********************************************************************         
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
* DSECT TO COVER TEMPORARY W/S                                                  
*                                                                               
RADWRKD  DSECT                                                                  
THISKEY  DC    XL20'00'                                                         
RADWRKX  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
         EJECT                                                                  
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DEARBCLO  02/13/12'                                      
         END                                                                    
