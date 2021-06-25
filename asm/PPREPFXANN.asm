*          DATA SET PPREPFXANN AT LEVEL 037 AS OF 07/02/01                      
*                                                                               
*     (1/13/97 CONVERTS BACKER (BS) ACC OFFICES                                 
*                                                                               
*        QOPT5   N= DON'T MARK FILE (EVEN IF WRITE=YES)                         
*                                                                               
*                                                                               
*PHASE PP0202U,+0,NOAUTO                                                        
*INCLUDE DDUCOM                                                                 
         TITLE 'PP0202 - TEST DDUCOM'                                           
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
*                                                                               
         XC    UCOMBLK,UCOMBLK                                                  
         LA    R2,UCOMBLK                                                       
         USING DDUCOMD,R2                                                       
         MVC   UCACOMF,VCOMFACS                                                 
         MVI   UCSYS,C'P'                                                       
***      MVC   UCPROG,=C'MX'       <- TEST MBI FILTER                           
         MVC   UCAGY,=C'SJ'                                                     
         MVI   UCMED,C'M'                                                       
         MVC   UCCLT,=C'AMC'                                                    
         MVC   UCPRD,=C'LX '                                                    
         MVC   UCEST,=H'001'                                                    
         MVC   UCDIV,=C'001'                                                    
         MVC   UCREG,=C'001'                                                    
         MVC   UCDST,=C'001'                                                    
         OI    UCOPT,UCOPRD+UCOEST+UCOREG+UCODST                                
         GOTO1 =V(DDUCOM),UCOMBLK                                               
*                                                                               
*        MVC   P+2(4),UCPEDITS                                                  
*        GOTO1 HEXOUT,DMCB,UCPMXLNS,P+10,4                                      
**       GOTO1 HEXOUT,DMCB,UCPLENS,P+20,4                                       
*        L     R4,UCPTTLS          PRD TITLES                                   
*        MVC   P+30(80),0(R4)                                                   
*        L     R4,UCPDATA          PRD DATA                                     
*        MVC   P2(128),0(R4)                                                    
*                                                                               
         MVC   P+2(4),UCDEDITS                                                  
         GOTO1 HEXOUT,DMCB,UCDMXLNS,P+10,4                                      
         GOTO1 HEXOUT,DMCB,UCDLENS,P+20,4                                       
         L     R4,UCDTTLS          EST TITLES                                   
         MVC   P+30(80),0(R4)                                                   
         L     R4,UCDDATA          EST DATA                                     
         MVC   PSECOND(128),0(R4)                                               
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
*                                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
RUNL     DS    0H                                                               
         B     EXIT                                                             
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
ELCODE   DS    X                                                                
UCOMBLK  DS    XL100                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDUCOMD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037PPREPFXANN07/02/01'                                      
         END                                                                    
